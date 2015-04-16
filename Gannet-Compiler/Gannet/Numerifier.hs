
-- |This module provides integer transformations for various types.
-- a Bool flag is used throughout to toggle numerification.

-- WV20110609 for old config handling, revert to before r4987 and undef NEW

module Gannet.Numerifier (
    numerify,
    newtestServiceGT, -- for appendIfLambda in Packetizer so OBSOLETE
    newnumService,
    newnumServiceGT,
    newnumServiceGL,
    getOpcode, -- from Gannet.SBA.Constants
    getSCId, -- from Gannet.SBA.Constants
    getSCLId, -- from Gannet.SBA.Constant
    getSNId -- from Gannet.SBA.Constants
) where

import Gannet.SBA.Constants
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
import Gannet.State.Scope
--import Gannet.Warning

--import qualified Data.Map as Hash

{-
For a K_S Symbol, the Name field must contain the class id and method id
In 64-bit it could also contain the node id, which would be a plus
-}
-- | Numerify the Name field of a GannetSymbol
numerify :: GannetSymbol -> Bool -> GannetSymbol
numerify gs num 
    | num == False = gs
-- The name of a K_L, K_A or K_D is stored in the Name field. However, to support 
-- LET services on different nodes (and in one go to make K_D more generic)
-- it would be best to have the Node ID as part of the name, at least for 64-bit    
--    | kind gs == K_L || kind gs == K_A || kind  gs == K_D =      
--        if isNumeric (name gs) -- means it has been numerified already. Where?
--            then
--                gs
--            else
--                let            
--                    numvar =
--                        case Hash.lookup (subtask gs) scopes of
--                            Just scoperec -> 
--                                case Hash.lookup (name gs) (varmap scoperec) of 
--                                    Just (_,n) -> n
--                                    Nothing -> 0 --  ad hoc!
--                            Nothing -> 0
----                    numname =     (GannetTokenL (GannetLabelI numvar))
--                    snid = warning (lookupNodeIdFromFQN (getGSNameStr (qualifyVarName gs))) ("Numerifier: SNId = "++(show snid)++" for "++(show gs)++"\n")
--                    numfqname= GannetTokenL (GannetLabelI ((shiftSNId snid)+numvar))
--                in
--                    gs{name=numfqname}
    | kind gs == K_S = 
        let
            numname = testGTS (name gs)
        in
            gs{name=numname}             
    | kind gs == K_R =
        let
            numname = testGTR (name gs)
        in
            gs{name=numname}             
    | otherwise = gs


--isNumeric :: GannetToken -> Bool
--isNumeric numname@(GannetTokenL (GannetLabelI sid)) = True
--isNumeric name = False


testGTS :: GannetToken -> GannetToken
testGTS (GannetTokenS sname) = GannetTokenS (newnumServiceS sname True)
testGTS (GannetTokenL sname) = GannetTokenL (newnumServiceS sname True)

testGTR :: GannetToken -> GannetToken
testGTR (GannetTokenS sname) = GannetTokenS (newnumServiceR sname True)
testGTR (GannetTokenL sname) = GannetTokenL (newnumServiceR sname True)
testGTR (GannetTokenB sname) = GannetTokenB sname

-- | Helper for numerification of Services    
-- Returns the service id if no aliases, the opcode otherwise.

{- 
    New numerifier for K_S
        The K_S in 64-bit gets the numerfied FQN        

-}
newnumServiceS :: GannetLabel -> Bool -> GannetLabel
newnumServiceS gl num
    | num == False = gl
    |otherwise =
        let
            GannetLabelS sname = gl
            (snid,sclid,scid,opcode,idx) = lookupFQNIds sname
            numvar = (shiftSNId (snid+idx))+(shiftSCLId sclid)+(shiftSCId scid)+opcode
        in
            GannetLabelI numvar

{-
The Name field of a Reference symbol contains the Node Id, that should be enough.
However, in 64-bit we have space so we might a well include the SCId and even the opcode
Which means that it becomes identical to newnumServiceS
For 32-bit it is only the node Id, i.e. the opposite of newnumServiceS
-}

newnumServiceR :: GannetLabel -> Bool -> GannetLabel
newnumServiceR gl@(GannetLabelI _) _ = gl -- not sure about this, but that would mean it was called twice.
newnumServiceR gl num
    | num == False = gl
    |otherwise =
        let
            GannetLabelS sname = gl
            (snid,sclid,scid,opcode,idx) = lookupFQNIds sname
            numvar = (shiftSNId (snid+idx)) +(shiftSCLId sclid)+(shiftSCId scid)+opcode
        in
            GannetLabelI numvar



-- Should return the Node Id, so we use lookupNodeIdFromFQN
newnumService:: GannetLabel -> Bool -> GannetLabel    
newnumService gl@(GannetLabelI _) _ = gl
newnumService gl@(GannetLabelS _) num
    | num == False = gl
        |otherwise =
            let
                GannetLabelS sname = gl
                numvar = lookupNodeIdFromFQN sname
            in
               GannetLabelI numvar

-- | Turn a Service name string into a GannetLabel
newnumServiceGL :: String -> Bool -> GannetLabel
newnumServiceGL s num=  newnumService (GannetLabelS s) num
{-
The question is, can I assume that if num is False, I can just return the GannetLabel?
I guess not: should create a FQN label and return that!    
So rather, what should this function do?
If num, look up FQN and return ids
else, look up FQN and return it.

What should be returned depends on the use:
    - for K_R, 64-bit, the FQN or its numeric value
    - for K_R, 32-bit, the SNId+SCId (num), or the FQN Str
    - For K_S, 64-bit, as K_R
    - for K_S, 32-bit, the SCId+Opcode , or the FQN Str
    - for the packet header, to/return_to, the SNId should be enough 
-}
--numServiceNodeGL :: String -> Bool -> GannetLabel
--numServiceNodeGL s num=  numServiceNode (GannetLabelS s) num

-- | Turn a Service name string into a GannetToken
newnumServiceGT :: String -> Bool -> GannetToken
newnumServiceGT s num = GannetTokenL (newnumServiceGL s num)

-- only used for testLambda in Packetizer, so probably OBSOLETE
newtestServiceGT :: GannetToken -> String -> Bool -> Bool
newtestServiceGT (GannetTokenL gl) = newtestServiceGL gl
newtestServiceGT (GannetTokenS gl) = newtestServiceGL gl
        
newtestServiceGL :: GannetLabel -> String -> Bool -> Bool
newtestServiceGL gl@(GannetLabelI glnum) sname num
-- here I must get the SCId and Opcode and compare them
    | num = -- error $ "newtestServiceGL: "++(show gl)++" <> "++(show sname)
        let
            (_,refsclid,refscid,refopcode,_) = lookupFQNIds sname
            scid = getSCId glnum
            sclid = getSCLId glnum
            opcode = getOpcode glnum
        in
            (sclid==refsclid)&&(scid==refscid)&&(opcode==refopcode)
    | otherwise = error $ "newtestServiceGL: The label "++(show gl)++" is already numerified\n" 
newtestServiceGL gl@(GannetLabelS glname) sname num 
    | num = 
        let
            (_,glsclid,glscid,glopcode,_) = lookupFQNIds glname
            (_,ssclid,sscid,sopcode,_) = lookupFQNIds sname
        in
            (glsclid==ssclid)&&(glscid==sscid)&&(glopcode==sopcode)
    | otherwise =
        let
            GannetLabelS nlname = gl
            (_,nlln,nlscn,nlmn,_) = lookupFQNStrs nlname 
            (_,sln,sscn,smn,_) = lookupFQNStrs sname 
        in
            (nlln==sln)&&(nlscn==sscn)&&(nlmn==smn)
            

