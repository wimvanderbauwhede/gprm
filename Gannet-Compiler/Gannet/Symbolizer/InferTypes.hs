{-# LANGUAGE CPP #-}
-- | Infer the type of the return value of a service
-- Unused at the moment, work in progress.

-- WV20110609 for old config handling, revert to before r4987 and undef NEW

module Gannet.Symbolizer.InferTypes (
--    inferTypes,
--    inferTypeBySymbol,
--    inferTypeByService,
    inferGDTbyArgs,
--    updateGDTinScope,
    lookupGSinScope,
    getGDT,
--    getLambdaArgs,
--    bindLambdaArg,
    reconcileTypes,
    testService,
    newtestService, -- NEW: rename!!!
    notQuote,
    updateVar,
    lookupVar    
) where 
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
import Gannet.State.Scope
import Gannet.State.Context
import Gannet.Numerifier

import qualified Data.Map as Hash

-- ----------------------------------------------------------------------------
{- | Infer type based on name of service    
- If not a control service, type is GData
- begin: Any. Look at the last expression *
lambda: Any. Depends on the body *
apply: Any. Depends on the body of lambda *
let: Any. But look at the last expression *

assign: Reference *
update: Reference *
read: Any, though I seriously consider restricting it to Symbol. Let's do that. *

if: Any. But use the rule above *
return: Any. Look at the arg *?

unsymbol: Data (obviously)
call: Depends on the arg


ALU: Number *

list: Reference. I think we might need a special ListRef kind... *
head: Symbol *
tail: Reference *
cons: Reference *
append: Reference *
length: Number *

buffer: Look at argument. Most likely Data (raison d'�tre)
stream: Look at argument of the buffer. Most likely Data
peek: Look at argument of the buffer. Most likely Data
get: Look at argument of the buffer. Most likely Data

GBI can be of type Number (Integer/Float/Bool) or String

A problem is that GLex and GArg can also have these types.

So I guess the best solution is to have a tuple-type with
the RetVal and the type info for the retval, ie
GDataType. OTOH, GDataType only makes sense if GRetVal is a Symbol
So maybe only GSymbol needs to be a tuple

Considering that a Gannet Symbol has all the necessary type info in the 
Kind and Datatype fields, where do we store the additional type info?

Assuming T_d to be used for GData and T_x for GAny, we can store that info
in the reference in the SLH. However, we can't store the Kind of the 
return value there. So it is better to have a separate retvaltype field in the SLH
At runtime, all we care about is T_I, T_F, T_S, T_b or T_d



Type inference:

-If it's a symbol it's trivial, unless it's a service symbol
-However, we should update the datatype field of the ref symbol in the SLH

-However 2, if a symbol has a definite type, we can use it to set the type in the SLH.
The same is true for an expression with a definite type.

LET/BEGIN
As it is not easy to know if a symbol will be the last to be appended to the list,
we can simply say that for let, we always set the SLH type to the symbol/expr type.
Consequenlty, the last symbol/expr to be appended will determine the type

RETURN
This is slightly more difficult. The type should be that of the first symbol to
be appended after the service symbol. So we should check the last element
of the SL via head $ reverse $ getSL st. If that is a K_S, it's OK

ASSIGN
if the symbol is NOT K_L (or K_Q) and it has a definite type, we can set this type for the READ SLH.
What we do is determine the type if the binding expression, then "back up" and modify the previous symbol;
to make the type available to READ, we should add the type information to the context, i.e. update the Gannet Symbol
in the context.

READ
if the symbol is K_L, it should be possible to find the corresponding ASSIGN and get the type from there.
A possible way is to make sure the Scope table is updated with the new type. Then we look up the scope table,
get the type, set it on th eK_L symbol and on the READ SLH

IF
Here we must know if it's not the condition expression. So if the last element of the list is K_S
then don't change the SLH else it's OK

LAMBDA
The return value of LAMBDA is (probably) that of the function body.
So if we're in LAMBDA and we encounter an expression, it's OK

APPLY
The return value of APPLY is that of the LAMBDA and thus that of the READ.


However, we should try to create the signature of the LAMBDA from the infered
types of APPLYs arguments.
 I guess we could take a similar approach to ASSIGN/READ: set the type on the K_A's in the scope.
 It's slightly more complicated: first we must get the LAMBDA expression to get the K_A's.
 This is difficult, I think it would be best to store LAMBDA expressions (e.g. in the context).
 The problem is that we don't really bind the K_L's at compile time, so we can't simply lookup
 the LAMBDA based on the variable.
(assign 'v (lambda 'x 'y ...))
(apply (read 'v) e1 e2 ...)

{ v => (lambda 'x 'y ...), ... }

So we should add a field to the Context, a Map between the full variable name i.e. (kind,subtask,name)
and the SymbolTree it stores.
In principle we could do the same for K_A values but in practice this is much more convoluted:
-get LAMBDA via the K_L
-get K_A's
-map to APPLY args
-store 
-and yes, we could at the same time set the types on the K_A's in the Scope. 

After doing all that, I think we need another iteration, so we walk the SymbolTree
again and this time we set the datatype of K_A symbols to the value in the ScopeTable.
But I'm afraid that is not enough. e.g.
(if (< n 1) '(return acc) ...)
We  can set the datatype for n and acc. Essentially it amounts to this:
K_A will be T_i, T_f, T_s or T_d but hopefully not T_x (as that would mean unresolved)
-}




-- NEW: looks like we only need to modify testService to make this FQN
inferGDTbyArgs :: SymbolTree -> SymbolTree -> GDataType
inferGDTbyArgs st st2 
    | (length $ getSL st) == 0 = T_x   
    | foldl1 (||) (map (newtestService slh) ["let","lettc","begin","seq"]) 
        = reconcileTypes (datatype nlabel) (datatype gs) 
    | foldl1 (||) (map (newtestService slh) ["return","returntc","apply","applytc"] ) 
        = let
            ms = head $ reverse $ getSL st
          in
            case ms of
                Symbol lgs -> if ((kind lgs) == K_S) then reconcileTypes (datatype nlabel) (datatype gs) else datatype nlabel
                _ -> datatype nlabel
-- the next bit sets the type on the ASSIGN expression. I does however not set the type on the
-- variable. The problem is that this requires both a change to st and to ctxt                
--    | newtestService slh "assign" 
--        = if (((kind gs) /= K_L) && ((kind gs) /= K_Q)) then reconcileTypes (datatype nlabel) (datatype gs) else datatype nlabel
-- (if p t f) => the type is that of t or f
-- so we check the last elt. If the one before it is a K_S, ignore.       
-- there is a small problem here: if will get the type of the last expression
-- unless that is T_x. So if that is T_i and the orig is T_f, there is a conflict 
    | newtestService slh "if"  || newtestService slh "iftc"
        = let
            ms = head $ reverse $ getSL st
          in
            case ms of
                Symbol lgs -> if ((kind lgs) /= K_S)  then reconcileTypes (datatype nlabel) (datatype gs) else datatype nlabel
                _ -> datatype nlabel
--    | newtestService slh "lambda" 
--        = if ((kind gs) /= K_A) then reconcileTypes (datatype nlabel) (datatype gs) else datatype nlabel
    | otherwise = datatype nlabel
    where
        gs = getGS st2          
        slh = getSLH st
        nlabel = label slh 

getGS (Symbol gs) = gs
getGS (SymbolList (_,slh)) = label slh


    
{-        
nt\ct T_x T_d T_i T_f T_s
T_x   T_x  ct  ct  ct  ct
T_d   nt  T_d ??  ??  ??
T_i   nt  ??  T_i T_f ??
T_f   nt  ??  T_f T_f ??
T_s   nt  ??  ??  ??  T_s

> d: Data, i.e. Any            000
> i: Integer                 001 -- 64-bit signed integer
> f: Float                    010
> c: Char                    011
> L: List of Data            100
> I: List of Integers        101
> F: List of Floats        101 (not that "Floats" are actually Doubles!
> s: String, List of Char    111                

data GDataType =  T_d | T_i | T_f | T_c | T_L | T_I | T_F | T_s | T_q | T_x | T_Error  

NOTE 27/01/2011
We don't need to do this here: this should happen in the HLL compiler!
-}
reconcileTypes :: GDataType -> GDataType -> GDataType 
reconcileTypes ct nt
    | nt == ct = ct
    | nt == T_x = ct
    | ct == T_x = nt
    | ct == T_i && nt == T_f = T_f
    | ct == T_f && nt == T_i = T_f
    | nt == T_q = ct
    | ct == T_q = nt
    -- this is debatable, but I think T_d is "weaker" than T_i|T_f|T_s
    | ct == T_d = nt
    | nt == T_d = ct
    | otherwise = T_Error -- error $ "Types " ++ (show nt) ++ " and " ++ (show ct) ++ " are incompatible"


-- This is a utility function solely used in the type checking code
-- NEW: sname and nlname are now FQN's, so typically the test is against the service class and method
-- e.g. "assign" becomes x.LET.assign; means any name stored in the slh should be a FQN
-- initially I'll use the Aliases to determing the FQN for all these
-- so we always test against FQNs
newtestService:: SymbolListHeader -> String -> Bool
newtestService slh sname = (nlscln==sscln)&&(nlscn==sscn)&&(nlmn==smn)
    where
        nlabel = label slh 
        nlname = getGSNameStr nlabel
        (_,nlscln,nlscn,nlmn,_) = lookupFQNStrs nlname 
        (_,sscln,sscn,smn,_) = lookupFQNStrs sname  
--        nl=splitTQN $ lookupFQN nlname
--        s=splitTQN $ lookupFQN sname
--        _:nlscln:nlscn:nlmn:[] = nl
--        _:sscln:sscn:smn:[] = s

testService :: SymbolListHeader -> String -> Bool
testService slh sname = nlname == GannetTokenL (GannetLabelS sname)         
    where
        nlabel = label slh 
        nlname = name nlabel 
        
notQuote :: SymbolTree -> Bool
notQuote (Symbol s) = (kind s)/=K_Q        
notQuote _ = True
-- get the GDatatype of a SymbolTree
getGDT :: SymbolTree -> GDataType
getGDT st =
    case st of
        Symbol gs -> datatype gs
        SymbolList (sl,slh) -> datatype $ label slh


updateGDTinScope :: ScopeTable -> GannetSymbol -> GDataType -> ScopeTable
updateGDTinScope scopes gs ndatatype =
    let
        gt = name gs
        cscope=subtask gs
        nscopes = case Hash.lookup cscope scopes of
            Just scoperec ->
                case Hash.lookup gt (varmap scoperec) of
                    Just (cgs,gsnum) -> 
                        let
                            rdatatype = reconcileTypes (datatype cgs) ndatatype
                            ngs=cgs{datatype=rdatatype}
                            nvarmap=Hash.insert gt (ngs,gsnum) (varmap scoperec)
                            nscoperec=scoperec{varmap=nvarmap}
                        in
                            Hash.insert cscope nscoperec scopes                                                                              
                    Nothing -> scopes -- error $ "InferTypes: Var "++(show gs)++" not in scope record.\n"
            Nothing -> scopes -- error $ "Scope "++(show cscope)++" not in scopes.\n"
    in
        nscopes                     



        
lookupGSinScope :: ScopeTable -> GannetSymbol -> GannetSymbol
lookupGSinScope scopes gs =
    let
        cscope=subtask gs
        gt = name gs 
        ngs = case Hash.lookup cscope scopes of
            Just scoperec ->
                case Hash.lookup gt (varmap scoperec) of
                    Just (tgs,_) -> tgs
                    Nothing -> gs -- error $ "InferTypes: Var "++(show gs)++" not in scope record.\n"
            Nothing -> gs -- error $ "Scope "++(show cscope)++" not in scopes.\n" -- errorGS         
    in
        ngs    

updateVar :: Context -> GannetSymbol -> SymbolTree -> Context
updateVar ctxt argsym ms = 
    let
        argv = symbolToGVarName argsym
        cvarbindings=varbindings ctxt
        nvarbindings= Hash.insert argv ms cvarbindings
        ndatatype = getGDT ms
        cscopes = scope ctxt
        nscopes = updateGDTinScope cscopes argsym ndatatype            
    in
        if kind argsym == K_Q 
            then error $ "UPDATE:" ++ (show argsym) ++ "=>" ++ (show ms)
            else ctxt{varbindings=nvarbindings,scope=nscopes}        

lookupVar :: Context -> GannetSymbol -> SymbolTree
lookupVar ctxt gs =
    let
        gv = symbolToGVarName gs   
    in
        case Hash.lookup gv (varbindings ctxt) of
            Just st -> st
            _ -> emptyST -- error $ "Symbol " ++ (show gs) ++ " is not in VarBindings:" ++ (show (varbindings ctxt))


