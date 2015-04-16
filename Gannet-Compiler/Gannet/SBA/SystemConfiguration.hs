{-# LANGUAGE CPP #-}

{- | The SystemConfiguration is the description of all the Services in the Gannet
System-on-Chip. Every service core can provide multiple services, these are accessed
via the Aliases.

WV08122010
The new approach is to use an object notation and model for the services. So instead of having aliases,
we now consider every service instance to be of some type Service, which is a kind of interface
that provides a number of methods. 

So, instead of (or rather, in addition to) looking up a "service" in the aliases, if the service instance name contains a
dot '.' we look up its type in the service instance record and use that to look up the method in the "Services" entry of the YML file


The SystemConfiguration YAML file contains the names and the numerical identifiers for all services.  
-}

-- WV20110609 for old config handling, revert to before r4987 and undef NEW

module Gannet.SBA.SystemConfiguration (

newservices,
serviceids,
newaliases,
newinterfaces,
isFQN,
isTQN,
isMC,
isAlias,
isServiceNode,
--splitFQN,
--splitTQN,
lookupFQN,
lookupFQNStrs,
--lookupPQN,
cmpFQN,
lookupFQNIds,
--lookupNodeId,
lookupNodeIdFromFQN,
--lookupClasses,
--checkClass,
--lookupClassId,
--lookupMethodId, 
--qualifyVarName,


) where
import qualified Data.Map as Hash
--import Data.List
--import Data.Maybe
import Gannet.Warning
--import Gannet.SBA.Types
import Gannet.SBA.ConfigReader
import {-# SOURCE #-} Main ( ymlFileName )

type ServiceNodes = Hash.Map String (Integer,[String])
-- WV: bit of a hack, these are language-specific services that the compiler needs to know about, a lot of them are obsolete, FIXME!!
noWarning = [] -- "let", "lettc", "return", "returntc", "apply", "applytc", "assign", "if", "iftc", "lambda","buf", "read", "stream"]

{-
With the new format for SystemConfiguration, we need a different lookup.
What do we need?
- the FQN for aliases, needed by Gannet/Symbolizer.hs
- The Service Node Id, needed for the packets, i.e for the packet header (to, return-to, return-as)
- The Service Class Id and the Opcode, needed for the service symbols
- check if a Service Node does provide an given Service Class
- check if a Service Class does provide an given Method

- For the 64-bit case, we need the FQN in the Name field!

-}
-- WV20150401 this should be obsolete
isFQN mfqn = length (filter (\c -> (c == '.')) mfqn) == 2 

-- WV20150401 what I need is aliases for object instances. So say we check if it's a method call
isMC mmc = length (filter (\c -> (c == '.')) mmc) == 1
-- WV20150401 then we lookup the instance alias so we split first
--splitMCN mmc  
--    | isMC mmc = split '.' mmc
--    | otherwise = error $ "splitMC: "++(show mmc)++" is not a method call"
    

-- "Totally Qualified Name", i.e. including the library name
isTQN mtqn = length (filter (\c -> (c == '.')) mtqn) == 3

splitFQN mfqn  
    | isFQN mfqn = split '.' mfqn
    | otherwise = error $ "splitFQN: "++(show mfqn)++" is not an FQN"
    
splitTQN mfqn  
    | isTQN mfqn = split '.' mfqn
    | otherwise = error $ "splitTQN: "++(show mfqn)++" is not a TQN"
    
-- if it's not an FQN, it must be an alias. But of course it could be  method call
isAlias ma = Hash.member ma newaliases

lookupFQN :: String -> String
lookupFQN name =
    let 
        (snn,scln,scn,mn,idx)=lookupFQNStrs name  
    in
        snn++"."++scln++"."++scn++"."++mn++"["++(show idx)++"]"


-- What about opcodes with []? I guess we need a "hasIndex" function ...
-- The index only matters when we numerify: then we need to convert the name to something numeric
-- WV20150401 I think the "mn++idx" is wrong because at this stage it must be a string. We need to add the idx as an additional value to the tuple!
lookupFQNStrs :: String ->  (String,String,String,String,Integer)
lookupFQNStrs name 
    | name == "_" = ("_","_","_","_",0)
    | isFQN name = 
        let 
            snn:scn:mn:[] = splitFQN basename 
            scln = lookupClassLibrary snn scn 
        in (snn,scln,scn,mn,idx)
    | isTQN name = 
        let 
            snn:scln:scn:mn:[] = splitTQN basename  
        in (snn,scln,scn,mn,idx)
    | isMC name =
        let            
            objinst:mn:[] = split '.' basename
        in    
            case Hash.lookup objinst newaliases of
                Just tqn -> 
                    let
                    snn:scln:scn:[] = split '.' tqn  
                    in 
                        (snn,scln,scn,mn,idx)                 
                Nothing -> if (name `elem` noWarning) then ("_","_","_","_",0) else warning ("_","_","_","_",0) ("Alias "++name++" is not defined in "++ymlFileName)            
    | otherwise =
        case Hash.lookup basename newaliases of
            Just tqn -> 
                let
                    snn:scln:scn:mn:[] = splitTQN tqn  
                in 
                    (snn,scln,scn,mn,idx)                 
            Nothing -> if (name `elem` noWarning) then ("_","_","_","_",0) else warning ("_","_","_","_",0) ("Alias "++name++" is not defined in "++ymlFileName)
    where
        (basename,idx) = splitIdx name

splitIdx :: String -> (String,Integer)            
splitIdx name = 
    let
        basename_idx = split '[' name
        basename = head basename_idx
        idx 
            | length basename_idx == 2  = -- means there is an index
                let
                    idx_str = init (basename_idx !! 1 ) -- drop the "]"
                in
                    read idx_str::Integer
            | otherwise = 0
    in
        (basename,idx)
-- this function takes an FQN and a string, and if they match, returns the string
--lookupPQN mfqn noerror = 
--    let
--        (_,_,c,m,_)=lookupFQNStrs mfqn noerror 
--    in
--        (c,m)

-- for let, we must check if the fqn is of class LET and method let, and similar for lambda 
-- If we can't find the fqn, it's false
cmpFQN :: String -> String -> Bool
cmpFQN "'" _ = False
cmpFQN fqn alias =
    let
        (_,l1,c1,m1,_)=lookupFQNStrs fqn 
        (_,l2,c2,m2,_)=lookupFQNStrs alias
    in
        (l1==l2 && c1==c2 && m1==m2) -- no check on idx as it belongs with the snid
--        if (l2=="_" && c2=="_" && m2=="_")
--            then
--                (l1==l2 && c1==c2 && m1==m2) 
--            else
                
        -- error $ l1++"=="++l2++" && "++c1++"=="++c2++" && "++m1++"=="++m2

isServiceNode fqn = Hash.member snn servicenodes where (snn,_,_,_,_) = lookupFQNStrs fqn 
  
lookupFQNIds name =
    let
        (nname,lname,scname,mname,idx) = lookupFQNStrs name  
    in
        if (nname,lname,scname,mname,idx) ==  ("_","_","_","_",0)       
            then
                --warning (0,0,0,0) ("Alias "++name++" not defined in .yml file!")
                (0,0,0,0,0) 
            else
                let                
                    snid =lookupNodeId nname
                    sclid=lookupSCLibId lname
                    scid =lookupClassId lname scname -- should be qualified with library name
                    opcode = lookupMethodId lname scname mname                    
                in
                    (snid,sclid,scid,opcode,idx) -- adding of snid and idx is done elsewhere 

-- WV: This is somehow not used in the Numerifier
lookupNodeIdFromFQN :: String -> Integer
lookupNodeIdFromFQN name =
    let
        tqn
            | isTQN name = name
            | otherwise = lookupFQN name
        tqnl = splitTQN tqn        
        nname = head tqnl        
        idx 
            | length tqnl == 4 = getIdx $ last tqnl
            | otherwise = 0
    in
        (lookupNodeId nname) + idx 

getIdx :: String -> Integer
getIdx mn = 
    let
        _:idx = split '[' mn
    in
        case idx of  
                [] -> 0
                [idx'] -> if (length idx'>0 && last idx' == ']') then read (init idx') else error $ "Illegal index "++(show idx)++" in SystemConfiguration::getIdx\n"
                _ -> error $ "Illegal index "++(show idx)++" in SystemConfiguration::getIdx\n"

lookupNodeId sn 
    | sn == "_" = 0
    | otherwise =
        case Hash.lookup sn servicenodes of
            Just (snid,_) -> snid
            Nothing -> error $ "lookupNodeId: Could not find a Service Node for "++sn++"\n"
            
lookupClassLibrary sn scn 
    | sn == "_" = "NONE"
    | otherwise =
        let
            sclns = case Hash.lookup sn servicenodes of
                Just (_,scls) -> scls
                Nothing -> error $ "lookupClassLibrary: Could not find a Service Node for "++sn++"\n"
            tmpl = filter (\t -> head t == scn ) (map (\scln -> split '.' scln) sclns)
            sclib = head (head tmpl)
        in
            sclib       
    

--lookupClasses sn 
--    | sn == "_" = []
--    | otherwise =
--        case Hash.lookup sn servicenodes of
--            Just (_,classes) -> classes
--            Nothing -> error $ "lookupClasses: Could not find a Service Node for "++sn++"\n"
--        
--checkClass sn classname = classname `elem` (lookupClasses sn ) 
        
lookupClassId libname classname  
    | libname == "_" = 0
    | classname == "_" = 0
    | otherwise =
--        case Hash.lookup classname newservices of
--            Just (scid,ctrl) -> scid
--            Nothing -> error $ "lookupClassId: Could not find a Service Class Id for "++classname++"\n"
        case Hash.lookup libname newservices of
            Just classes -> case Hash.lookup classname classes of
                    Just (scid,_) -> scid
                    Nothing -> error $ "lookupClassId: No such classname "++classname++" in Interfaces\n"       
            Nothing -> error $ "lookupClassId: No such library name "++libname++" in Interfaces\n" 
            
lookupMethodId libname classname method  = 
    case lookup base_method (zip methods methodids) of
        Just mid -> mid
        Nothing -> error $ "No such method: "++method++" for Service Class "++classname++"\n"
    where
    -- support indexing of services. 
--        base_method = method        
        base_method_idx = split '[' method
        base_method = head base_method_idx
--        idx 
--            | length base_method_idx > 1  = "["++(head (tail base_method_idx))
--            | otherwise = ""        
        methods = case Hash.lookup libname newinterfaces of
            Just classes->case Hash.lookup classname classes of
                    Just ml -> ml
                    Nothing -> error $ "lookupMethodId: No such classname "++classname++" in Interfaces\n"       
            Nothing -> error $ "lookupMethodId: No such library name "++libname++" in Interfaces\n" 
        methodids = take (length methods) (iterate (+1) 1) -- methods start at 1, so any 0 is an error
        
-- The danger with this is that it might try to qualify not only variables but also labels and buffers        
-- WV 04/02/2013: do we still need this? Vars should have globally unique numbers
--qualifyVarName :: GannetSymbol -> GannetSymbol
--qualifyVarName v 
--    | isFQN (getGSNameStr v) = v
--    | otherwise =
--        let
--            vname = getGSNameStr v
--            lettqn = lookupFQN "let"
--            nname:sclname:scname:_ = splitTQN lettqn
--            fqname=GannetTokenS (GannetLabelS (nname++"."++sclname++"."++scname++"."++vname))
--        in
--            v{name=fqname}        

servicenodes :: ServiceNodes
servicenodes = Hash.fromList servicenodelist
--(String, (Integer,[String]))
serviceids :: [Integer]
serviceids = map (\(_,(snid,_))->snid) servicenodelist

lookupSCLibId libname =
    case lookup libname (zip libraries sclibids) of
        Just sclid -> sclid
        Nothing -> error $ "No such library: "++libname++"\n"
    where
        sclibids = take (length libraries) (iterate (+1) 1)


newaliases = Hash.fromList newaliaslist -- [(alias,fqn)]
newservices = Hash.fromList (map (\t -> (fst t, Hash.fromList (snd t))) newservicelist)
newinterfaces = Hash.fromList (map (\t -> (fst t, Hash.fromList (snd t))) newinterfacelist)

--newservices = Hash.fromList newservicelist -- [ (libname,[(serviceclass, (serviceclassid,ctrl))]) ]
--newinterfaces = Hash.fromList newinterfacelist -- [ (libname,[(serviceclass,[method])]) ]

--alunames :: ALUNames
--alunames = Hash.empty





(libraries, servicenodelist, newaliaslist, newservicelist, newinterfacelist)= readSBAConfigNew ymlFileName -- defaults to SBA.yml

split :: Char -> [Char] -> [[Char]]
split delim str = words (map (\c-> (if delim==c then ' ' else c)) str)
