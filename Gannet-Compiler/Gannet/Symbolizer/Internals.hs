{-# LANGUAGE CPP #-}
{- |
Internals should contain only functions that will not have to be modified
when adding new features to the compiler.
That is the case for: 

> remapSubtask
> getAssignVarStack
> symKindDet
> symDatatype
> symExt
> symCount

Following functions are likely to change to accommodate Registers: 

> createSymCtxt 
> compileSym
> symKindEtc

-}

-- WV20110609 for old config handling, revert to before r4987 and undef NEW

module Gannet.Symbolizer.Internals (
    createSymCtxt,
    compileSym,
    symKindEtc,
    symKindDet,
    symDatatype,
    symExt,
    symCount
) where
import Gannet.SBA.Constants
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
import Gannet.State.Context
import Gannet.State.Scope
--import Gannet.Numerifier

import qualified Data.Map as Hash

{-
createSymCtxt creates the GannetSymbol corresponding to the Token x (passed as a TokenTree)
It also updates the current service, caller stack, current lambda and lambda stack.
To handle BUF etc, we also need Mode and Reg. So either we pass these on as extra
args, or we squirrel then into ctxt, or we make a new function
I think I'll go for the Context: simply add the previous symbol to the Context
That would be K_D:Mode=BUF:Register=(varname,regval)
-}
-- FQN OK
createSymCtxt :: TokenTree -> [TokenTree] -> Context -> (SymbolTree,Context)
createSymCtxt x tl ctxt =
    let
                -- compileSym compiles the GannetSymbol
                -- remapSubtask remaps the code address
                xgsym1=compileSym x K_Unknown tl ctxt  -- FQN                               
                (xgsym,ctxt2)
                    | (kind xgsym1)==K_Label =
                        let 
                            (skind,squot)=setLabelType xgsym1
                        in 
                            (xgsym1{kind=skind,quoted=squot},ctxt)  -- (xgsym1{kind=K_R},ctxt)
--                    | (kind xgsym1) == K_S || (kind xgsym1) == K_C = remapSubtask xgsym1 ctxt
                    | otherwise = (xgsym1,ctxt)
                skind=kind xgsym
                servicename = lookupFQN $ stringInToken x
                currentservice= -- FQN
                    if skind==K_S 
                        then servicename
                        else current ctxt
                callerservices= -- FQN
                    if skind==K_S
                        then
                            servicename:(callerstack ctxt)
                        else
                            callerstack ctxt
--                ncurrentlambda = currentlambda ctxt
--                    | cmpFQN (stringInToken x) "lambda" = subtaskc ctxt -- NEW: FQN!
--                    | otherwise = currentlambda ctxt
--                nlambdastack= lambdastack ctxt
--                    | cmpFQN (stringInToken x) "lambda" = (currentlambda ctxt):(lambdastack ctxt)
--                    | otherwise = lambdastack ctxt
                xsym = Symbol xgsym -- FQN
                -- LABEL handling
                -- the first K_S after the label triggers insertion of the label in the reflabelt: label => xgsym{kind=K_R}
                -- to handle quoted refs
                (nreflabelt,nreflabel) 
                    | (skind==K_S) && (reflabel ctxt /= emptyGT) = (Hash.insert (reflabel ctxt) xgsym{kind=K_R} (reflabelt ctxt),emptyGT)
                    | (skind==K_B) && (reflabel ctxt /= emptyGT) = (Hash.insert (reflabel ctxt) xgsym (reflabelt ctxt),emptyGT)
                    | otherwise = (reflabelt ctxt, reflabel ctxt)
                nctxt=ctxt2{    
                            current=currentservice,
                            callerstack=callerservices,
--                            currentlambda=ncurrentlambda,
--                            lambdastack=nlambdastack,
                            reflabel=nreflabel, -- not sure about this
                            reflabelt=nreflabelt,    
                            prevsym=emptyGS
                            }
    in
        (xsym,nctxt)            
        
        
setLabelType (MkGannetSymbol K_Label _  _ _ _ _  (GannetTokenB _) _ _ _ _) = (K_B,1) 
setLabelType gs@(MkGannetSymbol K_Label _  _ _ _ _  (GannetTokenL _) _ _ _ _) = (K_R,quoted gs)                
setLabelType gs@(MkGannetSymbol K_Label _  _ _ _ _  (GannetTokenS _) _ _ _ _) = (K_R,quoted gs) 

{-
-- remap the subtask using per-service address counters
                
remapSubtask :: GannetSymbol -> Context -> (GannetSymbol,Context)
remapSubtask gs ctxt =
    let
        sid = lookupServiceId (name gs)
        maddr = case Hash.lookup sid (addrcounters ctxt) of
                    Just a -> a
                    Nothing -> -1
        nctxt 
            | maddr>0 =
                let
                    naddr = maddr+1
                    ncounters=Hash.insert sid naddr (addrcounters ctxt)
                in
                    ctxt{addrcounters=ncounters}
            | otherwise = ctxt    
        addr
            | maddr>0 = maddr
            | otherwise = (subtask gs)
        
    in
        (gs{subtask=addr},nctxt)
-}

{-
If it's K_Unknown and not deterministic, it should be in the scope.
if skind == K_Unknown && symKindDet name == K_Unknown 
then -- look up in scope table
else
 
-}
-- FQN: if this token is a service, we should FQN it
-- not that we don't FQN variable names because at this stage we don't know if 
-- a GannetTokenL is a let-var, a label, a buf etc.

compileSym :: TokenTree -> GSymbolKind -> [TokenTree] -> Context -> GannetSymbol
compileSym token skind tl ctxt  
    | Hash.member name (reflabelt ctxt) = 
        case Hash.lookup name (reflabelt ctxt) of
            Just gs -> gs{kind=K_Label}
            Nothing -> errorGS        
    | skind == K_Unknown && symKindDet name == K_Unknown = 
            getGSfromScope name (letc ctxt) (getAssignVarStack name ctxt) (scope ctxt) 100
    | otherwise = 
        let
            (kind,subtask,lambda) = symKindEtc name skind ctxt
            datatype = symDatatype name
            ext = symExt name
            quoted=0            
            count
                | kind == K_L = lambda
                | otherwise = symCount tl kind
            gvsym = prevsym ctxt
            fqname
                | kind == K_S = GannetTokenS $ GannetLabelS $ lookupFQN (strFromToken name)
                --((\(GannetTokenS (GannetLabelS nstr)) -> nstr) name)
                | otherwise = name
        in            
            MkGannetSymbol kind datatype ext quoted task subtask fqname count lambda (mode gvsym) (reg gvsym)
    where
        Token name = token -- name :: GannetToken        
        task=taskc ctxt

strFromToken (GannetTokenS (GannetLabelS nstr)) = nstr
strFromToken other = error $ "This Token is not a Service Token: "++(show other)
                
getAssignVarStack :: GannetToken -> Context -> [Integer]
getAssignVarStack var ctxt = 
    case Hash.lookup var (assignvarstacks ctxt) of
        Just st -> st
        Nothing -> []                

{-
If it's not a specific keyword and the kind is K_Unknown, try to work out the kind.

-for ASSIGN, LAMBDA and DATA, the kind is passed in via skind
- K_B, K_Q and K_S are deterministic
- so what remains calls to K_L, K_A and K_D
-}
        
symKindEtc :: GannetToken -> GSymbolKind -> Context -> (GSymbolKind,Integer,Integer)
symKindEtc gt skind ctxt 
    | skind == K_Q = (K_Q,0,0)
    | (skind==K_Unknown) && (dkind/=K_Unknown) = -- deterministic
        let
            dsubtask
                | dkind == K_B = dsubtaskc -- We need unique identifiers for extended symbols
                | otherwise = dsubtaskc
        in
            (dkind,dsubtask,0)        
    | skind == K_D = (K_D,0,0)
    | skind == K_L = (K_L,dletc,dlambda)
    | skind == K_A = (K_A,dsubtaskc,dlambda)            
    | otherwise = (skind,dsubtaskc,dlambda) -- should not come here!
        
    where
        dkind=symKindDet gt -- see if it's K_S, K_Q or K_B
        dlambda = inLambda ctxt
        dletc = (letc ctxt)
        dsubtaskc = (subtaskc ctxt)

{-
Try to determine the Kind in a deterministic way first
-}

symKindDet :: GannetToken -> GSymbolKind
symKindDet gt = case gt of 
    (GannetTokenS _) -> K_S
    (GannetTokenB _) -> K_B
    (GannetTokenQ _) -> K_Q
    (GannetTokenL (GannetLabelS x)) -> 
        if
-- if it's a FQN and matches one of the ServiceNodes, or it's a valid alias
            ((isFQN x) && (isServiceNode x)) || (isAlias x) || (isMC x)
            then
                K_S
            else
                K_Unknown

{-
Rules for datatypes
Actually, we could do a complete type inference thing here, but that seems way over the top.
The reason for knowing the type is know what to do with a byte, essentially.
So we distinguish between String (ASCII for the moment), Int (signed! watch out!) and Float
-}        
symDatatype :: GannetToken -> GDataType
symDatatype gt = case gt of
    (GannetTokenB b) -> case b of
        (GannetBuiltinI _) -> T_i
        (GannetBuiltinF _) -> T_f
        (GannetBuiltinS _) -> T_s
    (GannetTokenQ _) ->  T_q
    (GannetTokenL _) ->  T_x
    (GannetTokenS _) ->  T_d

symExt :: GannetToken -> Int
symExt gt = case gt of
    (GannetTokenB b) -> case b of
-- let's sort this out: We have actually 24 bits out of 32 or 48 out of 64.     
--        (GannetBuiltinI i) -> if ((i>32767)||(i< -32766)) then 1 else 0 -- FIXME: only for 64-bit!
        (GannetBuiltinI i) -> if ((i>c_NBITS_NOT_EXT)||(i< 1-c_NBITS_NOT_EXT)) then 1 else 0 -- FIXME: only for 64-bit!
        (GannetBuiltinF _) -> 1
        (GannetBuiltinS _) -> 1
    (GannetTokenQ _) ->  0
    (GannetTokenL _) ->  0
    (GannetTokenS _) ->  0        
    
-- symCount contains a reference to "data", it is unlikely that I will re-introduce the DATA service or even the DATA construct    
symCount :: [TokenTree] -> GSymbolKind -> Integer
symCount [] _ = 0         
symCount tl@([_]) kind
    | (kind==K_S) = toInteger (length tl) 
    | kind==K_B = toInteger (length tl) -- rather 'ad hoc' but should do the job
    | otherwise = 1              
--    where
--        t=head tl                    
symCount tl kind 
    | (kind==K_S) = toInteger (length tl)
    | kind==K_B = toInteger (length tl) -- rather 'ad hoc' but should do the job
    | otherwise = 1

