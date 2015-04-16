{- |
This is module provides functions to read in the SBA.yml configuration file
and convert its content to the format used by SBA/ServiceConfiguration.hs
-}

{-
    0: { GATEWAY: [0, sba_GATEWAY, 1,[1,1]], Addr: 12 }
ServiceInstances
    Int: { String: [String, Int, String, Int], "Addr": Int }
    Service Id: { Service Core name: [Service Core Id, Core Function name, Nthreads,[T_init,T_proc_val]], 'Addr': Service NoC Address, 'Service': Service Type/Class }
    
transforms into:    
    serviceinstancelist = [ (Service Core name, (Service Id, Service Core Id, Service Type)), ... ]
    
Aliases
    String: [String, Int, Int]
    Alias Name: [Service Core name, Service Id, Opcode,[T_init,T_proc_val]]
    
transforms into:
    aliaslist = [ (Alias name,(Service Core name,Opcode)),... ]
    
    to lookup: Service Core Name => Service Core Id
    
Services
        String: { String: [Int, [Int, Int]] }    
    Service Type: { Method name: [Method Id, [Int, Int]] }
    
transforms into:
        servicelist :: [ (String, [(String, Int)] ) ]
        servicelist = [ (Service Type, [(Method Name, Method Id)] ) ]


ALU_Names
    String: String
    '+': 'ALU:plus'
    
    WV 14/01/2011: Implementing the new SystemConfiguration.yml format
    
 For SystemC, it seems both service id and address must be contiguous
 And we must have exactly 15 services!

 Service Id: { Service Instance name: [Service Core Id, Core Function name, Nthreads], Addr: Service NoC Address }
 Service Name is the "class" or "type" of the service. It is basically an alias for the Core Function name
 Service Core name is actually the name of a particulat instance of the service.

--- # SystemConfiguration.yml
System:
  Version: 2.0
  NServiceNodes: 10 # excluding gateway
  ServiceNodes:
    c1: [ 11, [Service1, IF, BEGIN] ]
    
  Services: # the last item indicates if it is a control service (1) or computational service (0)
    BEGIN: [ 11, ls_BEGIN, 1 ]

  Aliases:
  # Alias Name (case sensitive): FQN
    if: c4.IF.if
  
  ServiceClasses:
    LET: [ let, assign, read, update, lettc ]

The tables we need to create:
ServiceNodes => servicenodelist -- [(servicenode,(snid,[serviceclass])] 
Aliases => aliaslist -- [(alias,fqn)]
Services => servicelist -- [(serviceclass, scid, ctrl)]
ServiceClasses => interfacelist -- [(serviceclass,[method])]    
-}

{-

getYMap :: YamlNode -> [(YamlNode, [YamlNode])]

-}
module Gannet.SBA.ConfigReader (
    readSBAConfig,
    readSBAConfigNew,
    parseYamlFile,
    getSystemYMaps,
    serviceYMapToList,
    servicenodeYMapToList,
    aliasYMapToList,
    interfaceYMapToList,    
    namesYMapToList
) where

import Data.Yaml.Syck
import Data.Char (toLower)
import System.IO.Unsafe
-- for $GPRM_DIR
import System.Environment
import System.Directory
{-
First we read the Libraries field. 
Based on the Library names we need to find the corresponding config files,
so we somehow need to access $GPRM_DIR


 
check :: (FilePath -> IO Bool) -> FilePath -> IO ()
check p s = do
  result <- p s
  putStrLn $ s ++ if result then " does exist" else " does not exist"
 
main = do
  check doesFileExist "input.txt"
  check doesDirectoryExist "docs"
  check doesFileExist "/input.txt"
  check doesDirectoryExist "/docs"
  
Put that in getSBALibraryConfig

The "old New" way is to get

(snm,am,sm,scm) = getSystemYMapsNew yml
  i.e. we read ServiceNodes, Aliases, Services, ServiceClasses

In the "new New" way, we get ServiceNodes and Aliases from the AppConfig
and Services, ServiceClasses from the LibConfigs; 
the Services and ServiceClasses are prefixed with the Library name.

In the Gannet code, a lookup c4.IF.if now goes as follows:
- lookup the entry for c4 in ServiceNodes
- in the list, find the LibName.IF (filter)
- using this LibName, get the numbers from ServiceClasses and Services



-}
readSBAConfigNew appcfgfile =
    let
        (libraries, servicenodelist, newaliaslist) = readSBAConfigApp appcfgfile        
--        [(library, (newservices,newserviceclasses))]
        localpathlst = split '/' appcfgfile
        localpath=foldl1 (++) (map (\s->s++"/") (init localpathlst)) -- i.e. the path without the filename
        libcfglists = map (\ln->readSBAConfigLib ln localpath) libraries 
    in
        (libraries, servicenodelist, newaliaslist
        ,map (\t -> (fst t, fst (snd t))) libcfglists 
        ,map (\t -> (fst t, snd (snd t))) libcfglists
        )                 

readSBAConfigApp cfgfile =
    let
        yml = unsafePerformIO $ parseYamlFile cfgfile
        (lsm,snm,am) = getSystemYMapsApp yml
    in
        (librariesYMapToList lsm, servicenodeYMapToList snm,newaliasYMapToList am)

readSBAConfigLib libname localpath =
    let
        libcfgfile = unsafePerformIO $ findLibConfigPath libname localpath   
        yml = unsafePerformIO $ parseYamlFile libcfgfile
        (lm,sm,scm) = getSystemYMapsLib yml
    in
        (libraryYMapToList lm,(newserviceYMapToList sm,newinterfaceYMapToList scm))

-- This version stays in the IO monad rather than using unsafePerformIO
findLibConfigPath libname localpath = do
    evGPRM_DIR <- getEnv "GPRM_DIR"
    isLocal <- doesFileExist $ localpath++"/GPRM/"++libname++".yml"
    isLocalNew <- doesFileExist $ "./src/GPRM/Kernel/"++libname++".yml"
    isLocalNewGen <- doesFileExist $ "./gensrc/"++libname++".yml"
--    putStrLn ("./gensrc/"++libname++".yml")
--    putStrLn $ show isLocalNewGen 
    if isLocal
        then return $ localpath++"/GPRM/"++libname++".yml" 
        else if isLocalNew
            then return $ "./src/GPRM/Kernel/"++libname++".yml" 
            else if isLocalNewGen
                then return $ "./gensrc/"++libname++".yml" 
                else do
                    isGlobal <- doesFileExist $ evGPRM_DIR++"/GPRM/src/SBA/Base/"++libname++".yml"
                    if isGlobal
                        then return $ evGPRM_DIR++"/GPRM/src/SBA/Base/"++libname++".yml"
                        else error $ "findLibConfigPath: Can't find Library Config file "++libname++".yml, looked in:\n"++(unlines
                                [localpath++"/Gannet/","./src/GPRM/Kernel/","./gensrc/",evGPRM_DIR++"/GPRM/src/SBA/Base/"])
                    

readSBAConfig cfgfile =
    let
        yml = unsafePerformIO $ parseYamlFile cfgfile
        (sim,am,sm,nm) = getSystemYMaps yml
    in
        (flattenServiceList (serviceYMapToList sim),(aliasYMapToList am),(interfaceYMapToList sm),(namesYMapToList nm))

getSystemYMaps yml =
        let
                headnode = getYMap yml -- returns [(System, ...)]
                sysmaps = getYMap $ getYMapVal $ head headnode -- returns all maps, as a list [(YamlNode,YamlNode)]                
                serviceYMap_v = findYamlEntry "ServiceInstances" sysmaps 
                aliasYMap_v = findYamlEntry "Aliases" sysmaps
                interfaceYMap_v = findYamlEntry "Services" sysmaps
                namesYMap_v = findYamlEntry "ALU_Names" sysmaps
    in        
        (serviceYMap_v,aliasYMap_v,interfaceYMap_v,namesYMap_v)


getSystemYMapsApp yml =
    let
        headnode = getYMap yml -- returns [(System, ...)]
        sysmaps = getYMap $ getYMapVal $ head headnode -- returns all maps, as a list [(YamlNode,YamlNode)]
--        versionYMap_v = findYamlEntry "Version" sysmaps
        librariesYMap_v = findYamlEntry "Libraries" sysmaps     
        servicenodeYMap_v = findYamlEntry "ServiceNodes" sysmaps         
        aliasYMap_v = findYamlEntry "Aliases" sysmaps
    in        
        (librariesYMap_v,servicenodeYMap_v,aliasYMap_v)        

getSystemYMapsLib yml =
    let
        headnode = getYMap yml -- returns [(System, ...)]
        sysmaps = getYMap $ getYMapVal $ head headnode -- returns all maps, as a list [(YamlNode,YamlNode)]
--        versionYMap_v = findYamlEntry "Version" sysmaps
        libraryYMap_v = findYamlEntry "Library" sysmaps     
        serviceYMap_v = findYamlEntry "Services" sysmaps
        interfaceYMap_v = findYamlEntry "ServiceClasses" sysmaps
    in        
        (libraryYMap_v,serviceYMap_v,interfaceYMap_v)    
                
ymlMapToList helper m =            
    let
       ym = getYMap m
    in
       map (\mv->helper mv ) ym       
              
librariesYMapToList m = map getYStr (getYSeq m)                                    
libraryYMapToList m = getYStr m
-- [(servicenode,(snid,[serviceclass])]  from (servicenode,(snid,[serviceclass]))
-- I'm assuming we have a Seq of Nodes
servicenodeYMapToList = ymlMapToList servicenodeYMapToList_helper
servicenodeYMapToList_helper (mk,mv)  =  
    let
        snn=getYStr mk
        snid_yn:scs_yn:[]=getYSeq mv
        snid = read (getYStr snid_yn)::Integer
        serviceclasses= map getYStr (getYSeq scs_yn)
    in        
        (snn,(snid,serviceclasses))
-- [(serviceclass,[wrapper, scid, ctrl]] ->  [(serviceclass, (scid, ctrl))]
newserviceYMapToList = ymlMapToList newserviceYMapToList_helper
newserviceYMapToList_helper (mk,mv)  =  
    let
            scname=getYStr mk
            scid_yn:_:ctrl_yn:[] = getYSeq mv
            scid = read (getYStr scid_yn)::Integer
            ctrl = read (getYStr ctrl_yn)::Integer
    in        
        (scname,(scid,ctrl))

newaliasYMapToList = ymlMapToList newaliasYMapToList_helper
newaliasYMapToList_helper (mk,mv)  = 
        let
                aliasname = getYStr mk
                fqn = getYStr mv
        in
                (aliasname,fqn)

newinterfaceYMapToList = ymlMapToList newinterfaceYMapToList_helper
newinterfaceYMapToList_helper (sctype,smvl)  = (getYStr sctype, map getYStr (getYSeq smvl)) 
   
serviceYMapToList = ymlMapToList serviceYMapToList_helper
serviceYMapToList_helper (mk,mv)  =  
    let
        sid=read (getYStr mk)::Integer
        scids=getServiceCoreIdsH mv
        sids=replicate (length scids) sid
        snames=getServiceNamesH mv
        stypes=getServiceTypesH mv        
    in        
        zip snames (zip (zip sids scids ) stypes) -- [(sname, ( (sid,scid),stype) )]

flattenServiceList = flattenServiceList_helper []

flattenServiceList_helper fsl sl = 
    if length sl==0
        then fsl
        else 
            let
                hsl:tsl=sl
                nfsl=fsl++hsl
            in
                flattenServiceList_helper nfsl tsl

-- was: 0: [GATEWAY, 10, sba_GATEWAY, 1, 12], i.e. mv is a list
-- is now: 0: { GATEWAY: [10, sba_GATEWAY, 1,[t1,t2]], GATEWAY2: [11, sba_GATEWAY2, 1,[t1,t2]], Addr: 12 }, i.e. mv is a map
-- transform into: (GATEWAY, (0, 10)), (GATEWAY2, (0,11)), ... ] ; timing info is ignored

getServiceNamesH l  =   
    let
        sm = getYMap l -- [ ( GATEWAY, [10, sba_GATEWAY, 1]), (GATEWAY2, [11, sba_GATEWAY2, 1]), (Addr, 12) ]        
        smf = filter (\kv -> let (k,_)=kv in (getYStr k)/="Addr" ) sm
        smks = map (\mkv -> getYMapKey mkv) smf
    in
        map (\mk -> map toLower (getYStr mk)) smks        

        
getServiceTypesH l = 
    let
        sm = getYMap l
        smf = filter (\kv -> let (k,_)=kv in (getYStr k)/="Addr") sm
        smvs = map (\mkv -> getYMapVal mkv) smf
    in
        map (\mv -> getYStr $ head $ getYSeq mv) smvs -- this should yield a list of Service Types for a particular Service ID, i.e. [String]


getServiceCoreIdsH l = 
    let
        sm = getYMap l
        smf = filter (\kv -> let (k,_)=kv in (getYStr k)/="Addr" ) sm
        smvs = map (\mkv -> getYMapVal mkv) smf
    in
        map (\mv -> read (getYStr ( (getYSeq mv) !! 1))::Integer) smvs


--getServiceName l  =   
--    let
--        sl = getYSeq l
--    in
--        map toLower $ head $ map (\lv -> getYStr lv) sl

--getServiceCoreId l = 
--    let
--        sl = getYSeq l
--        sls = map (\lv -> getYStr lv) sl
--    in
--        read (sls !! 1)::Integer
   
aliasYMapToList = ymlMapToList aliasYMapToList_helper
aliasYMapToList_helper (mk,mv)  = (map toLower (getYStr mk), getAliasTuple mv)

-- 'plus': [ALU, 7, 9,[t1,t2]] => ("plus",("alu",7,9)) ; timing info is ignored 
getAliasTuple l =
        let
        sl = getYSeq l 
        sls = map (\lv -> getYStr lv) sl -- all strings : ["ALU","7","9",["t1","t2"]]
    in
        (map toLower (sls !! 0), read (sls !! 1)::Integer, read (sls !! 2)::Integer)
        
--Services
--        String: { String: [Int, [Int, Int]] }    
--    Service Type: { Method name: [Method Id, [Int, Int]] }
--    
--transforms into:
--        servicelist :: [ (String, [(String, Int)] ) ]
--        servicelist = [ (Service Type, [(Method Name, Method Id)] ) ]        
        
interfaceYMapToList = ymlMapToList interfaceYMapToList_helper
interfaceYMapToList_helper (sctype,smvl)  = (getYStr sctype, getInterfacePairList smvl) 

-- smvl is the list of methods for each service type,  
getInterfacePairList :: YamlNode -> [(String,(Integer,Integer))]
getInterfacePairList smvl =
        let        
--                ynl = getYSeq smvl -- [YamlNode]
                ynm = getYMap smvl -- [YamlNode]
                sid 
                        | length ynm ==0 = error $ "YAML: No interfaces found: "++( show ynm)
                        | otherwise = getServiceId ynm
                --sid = getServiceId ynl
                rsmvl = removeServiceId ynm
        in
                map (getInterfacePair sid) rsmvl 

getInterfacePair sid smv =
        let
                --mm = head $ getYMap smv -- head [(YamlNode, [YamlNode])]
                mn= getYStr $ fst smv -- getYMapKey mm -- mm::(YamlNode, YamlNode)
                mids = getYStr $ head $ getYSeq $ snd smv --getYMapVal mm
                tl = reads mids::[(Integer, String)] -- ::Integer
                (mid_s,rest)=head tl
                mid
                        | length tl > 1 = error $ "YAML: "++(show tl)
                        | rest /= "" = error $ "YAML: "++rest
                        | otherwise = mid_s                
        in
                (mn,(sid,mid))
                
getServiceId :: [ (YamlNode,YamlNode) ] -> Integer
getServiceId ynl = 
        let
                l = filter (not . isMethod) ynl
                serviceidstr 
                        | length l /= 1 = error $ "YAML: "++(show ynl)
                        | otherwise = getYStr $ snd $ head l
                pl = reads serviceidstr::[(Integer,String)]
                res
                        | length pl /=1 = error $ "YAML: No ServiceId found: "++(show ynl)
                        | otherwise = fst $ head $ pl
        in
                res
                
removeServiceId :: [ (YamlNode,YamlNode) ] -> [ (YamlNode,YamlNode) ]
removeServiceId l = filter isMethod l

isMethod :: (YamlNode,YamlNode) -> Bool 
isMethod (k,_) = 
        let
                --mm = head $ getYMap yn -- head [(YamlNode, [YamlNode])]
                mn= getYStr k -- [(YamlNode, YamlNode)]                
        in
                mn /= "ServiceId"
                
namesYMapToList = ymlMapToList namesYMapToList_helper
namesYMapToList_helper (mk,mv)  = (map toLower (getYStr mk), map toLower (getYStr mv))

getYMapKey :: (YamlNode,YamlNode) -> YamlNode
getYMapKey (mk,_) = mk

getYMapVal :: (YamlNode,YamlNode) -> YamlNode
getYMapVal (_,mv) = mv

-- given a node, return as a list of tuples
getYMap :: YamlNode -> [(YamlNode,YamlNode)]
getYMap m = 
    let
        EMap sm = (n_elem m)
    in
        sm
        
-- given a node, return as a list
getYSeq :: YamlNode -> [YamlNode]
getYSeq l =
    let
        ESeq sl = (n_elem l)
    in
        sl
        
-- given a node, return as a string        
getYStr :: YamlNode -> String  -- k :: Data.ByteString.Char8 -- as Buf
getYStr mk = 
    let 
        EStr k = n_elem mk
    in
        unpackBuf k

findYamlEntry :: String -> [(YamlNode,YamlNode)] -> YamlNode
findYamlEntry entrykey entries =  
        let
                cands =  filter (\elt -> (getYStr (getYMapKey elt)) == entrykey) entries
                entryval
                        | length cands==1 = getYMapVal (head  cands)
                        | otherwise = error $ "YAML: no match for key "++entrykey++":"++(show cands)
        in
                entryval 
 
split :: Char -> [Char] -> [[Char]]
split delim str = words (map (\c-> (if delim==c then ' ' else c)) str)
