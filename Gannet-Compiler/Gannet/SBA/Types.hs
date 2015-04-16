-- |Fundamental Gannet types and utility functions for manipulating them.
module Gannet.SBA.Types (
-- * Types
-- ** TokenTree
TokenTree(..),
GannetToken(..),
GannetLabel(..),
GannetBuiltin(..),
-- ** SymbolTree
SymbolTree(..),
SymbolListHeader(..),
-- ** Gannet Symbol and fields
GannetSymbol(..),
GSymbolKind(..),
GMode(..),
GRegister,
GDataType(..),
GPacketType(..),
GannetName(..),
GVarName,
symbolToGVarName,
-- ** Gannet Packet etc
PacketList,
PacketRefList,
GannetPacket,
GannetHeader(..),
-- * Helper functions
addToken,
isTokenList,
lengthTL,
stringInToken,
getSL,
getSLH,
getS,
maybeS,
isSL,
isS,
extendGS,
getGSNameStr,
getStrFromToken,
-- * Constants
emptyST,
emptySL,
emptySLH,
emptyTL,
emptyGH,
emptyGPL,
emptyGReg,
emptyGS,
nullGS,
emptyGT,
quoteGS,
errorGS,
debugGS,
GRetVal(..),
GSymbol
) where 

import Gannet.SBA.Constants 
import qualified Data.Map as Hash
--------------------------------------------------------------------------------
-- 
--                    Types for Gannet
--
--------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- |A simple type for the intermediate tree
-------------------------------------------------------------------------------
data TokenTree = Token GannetToken | TokenList [TokenTree]
    deriving (Ord,Eq)
showT :: TokenTree -> String
showT (Token name) = show name
showT (TokenList contents) = "(" ++ unwordsList contents ++ ")"
unwordsList :: [TokenTree] -> String
unwordsList = unwords . map showT
instance Show TokenTree where show = showT

addToken :: TokenTree -> TokenTree -> TokenTree
addToken (TokenList v1) (TokenList v2) = (TokenList (v1++v2)) 
addToken v1s@(Token _) (TokenList v2) = (TokenList ([v1s]++v2))
addToken (TokenList v1) v2s@(Token _) = (TokenList (v1++[v2s]))     
addToken v1s@(Token _) v2s@(Token _) = (TokenList [v1s,v2s])

isTokenList :: TokenTree -> Bool
isTokenList (TokenList _) = True
isTokenList (Token _) = False 

lengthTL :: TokenTree -> Int
lengthTL (TokenList t) = length t
lengthTL (Token _) = 0

stringInToken :: TokenTree -> String
stringInToken (Token (GannetTokenS (GannetLabelS str))) = str
stringInToken (Token (GannetTokenL (GannetLabelS str))) = str
stringInToken (Token t) = show t
stringInToken (TokenList _) = ""

-- | Used in TokenTree and for the name field in GannetSymbol
data GannetToken = GannetTokenB GannetBuiltin | GannetTokenQ String | GannetTokenL GannetLabel | GannetTokenS GannetLabel 
    deriving (Ord,Eq)
showGT :: GannetToken -> String
showGT (GannetTokenB contents) = show contents
showGT (GannetTokenL (GannetLabelS contents)) = contents
showGT (GannetTokenS (GannetLabelS contents)) = contents
showGT (GannetTokenL (GannetLabelI contents)) = showFQId contents
showGT (GannetTokenS (GannetLabelI contents)) = showFQId contents
showGT (GannetTokenQ contents) = contents
instance Show GannetToken where show = showGT

showToken :: GannetToken -> String
showToken (GannetTokenB contents) = "GTB: <"++(show contents)++">\n"
showToken (GannetTokenL contents) = "GTL: <"++(show contents)++">\n"
showToken (GannetTokenS contents) = "GTS: <"++(show contents)++">\n"
showToken (GannetTokenQ contents) = "GTQ: <"++(show contents)++">\n"

showFQId num = (show $ getSNId num)++"."++(show $ getSCLId num)++"."++(show $ getSCId num)++"."++(show $ getOpcode num)

emptyGT :: GannetToken
emptyGT = GannetTokenL (GannetLabelS "")
{-
GannetName: NOT USED
-if it's a Builtin, make sure we have the right encoding
-if it's a Label, it can be numerified
So apart from the redundant Quote, it's the same as GannetToken
-}
data GannetName = GannetNameB GannetBuiltin | GannetNameL GannetLabel
    deriving (Ord,Eq)
showGN :: GannetName -> String
showGN (GannetNameB contents) = show contents
showGN (GannetNameL contents) = "<" ++ (show contents) ++ ">"
instance Show GannetName where show = showGN

data GannetBuiltin = GannetBuiltinI Integer | GannetBuiltinF Double | GannetBuiltinS String
        deriving (Ord,Eq)

showGB :: GannetBuiltin -> String
showGB (GannetBuiltinS contents) = "\"" ++ contents ++ "\"" 
showGB (GannetBuiltinI contents) = show contents
showGB (GannetBuiltinF contents) = show contents
instance Show GannetBuiltin where show = showGB

data GannetLabel = GannetLabelS String | GannetLabelI Integer
    deriving (Eq,Ord)
showGL :: GannetLabel -> String
showGL (GannetLabelS s) = s
showGL (GannetLabelI i) = show i
instance Show GannetLabel where show = showGL

emptyTL :: TokenTree
emptyTL = (TokenList [])
-------------------------------------------------------------------------------
-- | The actual Gannet Symbol Tree
-------------------------------------------------------------------------------
data SymbolTree = Symbol GannetSymbol | SymbolList ([SymbolTree],SymbolListHeader)
showST :: SymbolTree -> String
showST (Symbol s) = (show s) ++ "\n"
showST (SymbolList (contents,slh)) = "(\n" ++ unwordsSList contents ++ ") "++(show slh)++"\n" 
unwordsSList :: [SymbolTree] -> String
unwordsSList = unwords . map showST
instance Show SymbolTree where show = showST

-- |Get the [SymbolTree] list out of a ([SymbolTree],SymbolListHeader) tuple
getSL :: SymbolTree -> [SymbolTree]
getSL st = 
    let 
        (SymbolList (sl,_))=st
    in 
        sl
 
-- |Unwrap a GannetSymbol from a SymbolTree               
getS :: SymbolTree -> GannetSymbol            
getS st =
    let 
        (Symbol s)=st
    in 
        s
-- | Not a real Maybe, returns emptyGS if the arg is not a GannetSymbol
maybeS :: SymbolTree -> GannetSymbol
maybeS st2 =
    case st2 of    
        Symbol gs -> gs
        _ -> emptyGS
        
isSL :: SymbolTree -> Bool
isSL st =
    case st of
        SymbolList _ -> True
        _ -> False    

isS :: SymbolTree -> Bool    
isS st =
    case st of
        Symbol _ -> True
        _ -> False         
-- |Get the SymbolListHeader list out of a ([SymbolTree],SymbolListHeader) tuple        
getSLH :: SymbolTree -> SymbolListHeader
getSLH st = 
    let 
        (SymbolList (_,gh))=st
    in 
        gh
        
emptySL :: SymbolTree
emptySL = (SymbolList ([],emptySLH))

emptyST = Symbol emptyGS
-- | Auxiliary type for the list header in a Symbol Tree
data SymbolListHeader =     MkSymbolListHeader
                        {                        
                        lto            :: GannetLabel
                        ,lfrom        :: GannetLabel
                        ,label         :: GannetSymbol
                        ,rlambda     :: Integer -- I thought this was used to identify expressions with lambda-vars, but it's broken                        
                        ,to_apply   :: Bool -- Looks like I might get a lot of redundancy: only expressions with lambda-vars need to be send to APPLY
                        ,tagged        :: Bool -- IIRC this is for use with LABEL
                        ,retval         :: GRetVal
                        }
showSLH :: SymbolListHeader -> String                    
showSLH slh= "[" ++ (show (lto slh)) ++ "," ++ (show (lfrom slh)) ++ ",<" ++ (show (label slh)) ++ ">,L:"++(show (rlambda slh)) ++ ","++(show (to_apply slh)) ++ "," ++ (show (tagged slh)) ++ "]<" ++ (show (retval slh)) ++ ">"
instance Show SymbolListHeader where show = showSLH
                     
emptySLH :: SymbolListHeader
emptySLH     = (MkSymbolListHeader (GannetLabelS "_") (GannetLabelS "_") emptyGS 0 False False (GSV (K_Unknown,T_x)) )

--------------------------------------------------------------------------------
-- | Gannet Symbol 
--------------------------------------------------------------------------------
data GannetSymbol = MkGannetSymbol 
                    {
                        kind        :: GSymbolKind,
                        datatype    :: GDataType,
                        ext         :: Int,
                        quoted      :: Int,                         
                        task        :: Int,
                        subtask     :: Integer,
                        name        :: GannetToken,
                        count       :: Integer,
                        lambda        :: Integer,
                        mode        :: GMode,
                        reg         :: GRegister                        
                    } 
    deriving (Eq,Ord)
--                         0        | 1     | 2     | 3
data GMode = M_normal | M_var | M_buf | M_eos 
    deriving (Show,Eq,Ord,Enum) -- assuming Acc | Cache are synonyms for Var; Stream for Buf; Get | Peek are requests

type GRegister = (GannetToken, Integer)
emptyGReg = (emptyGT,0)
    
showGS :: GannetSymbol -> String
showGS (MkGannetSymbol K_Unknown _ _ _ _ _ _ _ _ _ _) = "0"
showGS s =
    let 
         n = name s
--         mode_reg
--            | kind s == K_S =":<"++(show (mode s))++":"++(show (reg s))++">"
--            | kind s == K_D =":<"++(show (mode s))++":"++(show (reg s))++">"
--            | otherwise = ""
    in
        (showGSK (kind s)) ++ ":" ++ (showGDT (datatype s)) ++ ":" ++ (showX (ext s)) ++":" ++ (showQ (quoted s)) ++ ":" ++ (show (task s)) ++ ":" ++ (show (subtask s)) ++ ":" ++ (show n) -- ++ "\t\t\t\t\t\t\t\t;C:" ++ (show (count s))  ++ ";L:" ++ (show (lambda s))++mode_reg 
showQ q
    | q==1 ="Q"
    | q==0 =" "
showX x
    | x==1 = "X"
    | x==0 = " "
        
instance Show GannetSymbol where show = showGS

getGSNameStr gs = getStrFromToken $ name gs
                
getStrFromToken (GannetTokenS (GannetLabelS namestr)) = namestr
getStrFromToken (GannetTokenS (GannetLabelI _)) = error $ "getStrFromToken: Service Token was already numerified"
getStrFromToken (GannetTokenL (GannetLabelS namestr)) = namestr
getStrFromToken (GannetTokenL (GannetLabelI _)) = error $ "getStrFromToken: Label Token was already numerified"
getStrFromToken gt = error $ "getStrFromToken: Not a Service or Label Token: "++(showToken gt)

pow2 :: Integer -> Integer
pow2 0 = 1
pow2 n = 2*(pow2 (n-1))

namebits :: Integer
namebits=32

namesize :: Integer
namesize=pow2 namebits -1

numeric=False
{-
K_D|T_x => 001|x, 1
K_A|T_x => 010|x, 2 # because S_APPLY==2 
K_L|T_x => 011|x, 3 # because S_LET==3
K_R|T_x => 100|x, 4
K_C|T_x => 101|x, 5  
K_B|T_x => 11|xx, so K_B actually spans 6 and 7 (not for 64-bit!)
-}


-- At the very very least we need B, R, A, L => 1,2,3,4 = 3 bits! 000->111
-- Alas, we also need K_D and K_C, so 4 bits
--                                   000   001   010   011   100   101   110   111  1000  1001  1010      1011
--                 0     1     2     3     4     5     6     7     8     9     10        11          12        13
data GSymbolKind = K_S | K_D | K_A | K_L | K_R | K_C | K_B | K_Q | K_E | K_X | K_Label | K_Unknown | K_Debug | K_Lref
    deriving (Show,Eq,Ord,Enum)
showGSK :: GSymbolKind -> String 
showGSK k
    | k==K_Unknown = "0"  
    | numeric = show $ fromEnum k     
    | otherwise = k'
        where 
            _:_:k'= show k   
{-

- The _essential_ types are integer, float and string, so 2 bits, let's say 00 means any
- A list type is nice too
- I'm not sure we need a Lambda type, as symbols of this type should never be passed to agnostic services
- We don't really need Bool nor Quote
 
-}
{- | Gannet DataTypes

> i: Integer
> f: Float
> s: String
> b: Bool (unused. use T_i) 
> d: Data, means "don't know, don't care"
> q: Quote -- Obsolete?
> l: List, a list of non-extended symbols
> L: Lambda
> x: Any

-}
--                0     1     2     3     4(0)  5(1)  6(0)  7(1)  8(0)
--data GDataType =  T_i | T_f | T_s | T_b | T_d | T_l | T_L | T_q | T_x | T_Error
{- | Gannet DataTypes
New proposal, for 64-bit of course. 
> d: Data, i.e. Any                        000
> i: Integer                                 001 -- 64-bit signed integer
> f: Float                                        010
> c: Char                                        011
> L: List of Data                        100
> I: List of Integers                101
> F: List of Floats                101 (not that "Floats" are actually Doubles!
> s: String, List of Char        111                                

> q: Quote -- only used by the compiler -- 8
> x: Unknown -- only used by the compiler -- 9, becomes 1
> Error: only used by the compiler

For 32-bit, we only have one bit, 2 bits for K_B 
So for 32-bit we can encode d,i,f,c but what we can do is say that
if it's extended, we use L,I,F,S
-}

dataTypes = ["d","i","f","c","L","I","F","s","q","x","Error"]
data GDataType =  T_d | T_i | T_f | T_c | T_L | T_I | T_F | T_s | T_q | T_x | T_Error  
    deriving (Show,Eq,Ord,Enum)
showGDT :: GDataType -> String 
--showGDT = show . fromEnum
showGDT dt = dataTypes !! (fromEnum dt)    
--    | numeric = show . fromEnum      
--    | otherwise = show  
    
emptyGS :: GannetSymbol 
emptyGS = MkGannetSymbol K_Unknown T_x    0 0 0 0 (GannetTokenL (GannetLabelS "_")) 0 0 M_normal emptyGReg
-- this symbol, numerified, should return 0
nullGS :: GannetSymbol
nullGS = MkGannetSymbol K_Unknown T_x  0 0 0 0 (GannetTokenB (GannetBuiltinI 0)) 0 0 M_normal emptyGReg

errorGS :: GannetSymbol
errorGS = MkGannetSymbol K_E T_x 0 0 0 0 (GannetTokenQ "ERROR") 1 0 M_normal emptyGReg
debugGS :: String -> GannetSymbol
debugGS msg = MkGannetSymbol K_Debug T_x 0 0 0 0 (GannetTokenQ msg) 1 0 M_normal emptyGReg
quoteGS :: GannetSymbol
quoteGS = MkGannetSymbol K_Q T_q 0 0 0 0 (GannetTokenQ "'") 1 0 M_normal emptyGReg -- quoteGS doesn't need task as it's a compiletime construct
-- rootGS = (GannetSymbol K_R T_x 0 0 0 0 (GannetTokenL "ROOT") 1 0)

isGannetBuiltin :: GannetToken -> Bool
isGannetBuiltin gt =
    case gt of
        GannetTokenB _ -> True
        _ -> False
    
-- | Turn a Built-in into an Extended symbol.

-- WV11032008: we'll use the Subtask field to hold the number of words
extendGS :: GannetSymbol -> [GannetSymbol]
extendGS gs 
    | isGannetBuiltin (name gs) =
        let 
            GannetTokenB gtb =  name gs
--            one=GannetTokenB (GannetBuiltinI 1)
            zero=GannetTokenB (GannetBuiltinI 0)
            nonext=True -- if True, int16 (int32 for 64-bit) values are not Extended            
        in
            case gtb of
                GannetBuiltinI gi -> 
                    if nonext && ((gi>=0 && gi<=namesize)|| (gi<0 && -gi<=namesize)) -- (gi<0 && gi> -1*namesize) simpler to only have uint16 in name field
                        then
                            [gs]
                        else -- integer value larger than field
                            let
                                gsn=gs{ext=1,name=zero,subtask=1}
                            in
                                gsn:[extGS gs]
                GannetBuiltinF _ -> 
                    let
                        
                        gsn=gs{ext=1,name=zero,subtask=1}
                    in
                        gsn:[extGS gs]
                GannetBuiltinS gstr ->
                    let 
                        (strlength,padding)= nwords gstr
                        gtstrlength=GannetTokenB (GannetBuiltinI strlength)
--                        strl=GannetTokenB (GannetBuiltinI strlength)
-- WV20110610: this is sub-optimal for 64-b, the string length is limited by the Subtask field
-- it would be better to use dedicated fields: the padding is at most 7 so 3 bits is enough
-- for 32-bits, using name for the length means max. 1024 characters, but atm. I don't care. 
                        gsn=gs{ext=1,name=gtstrlength,subtask=padding}                        
                        strlist= map extGSstr (splitInWords gstr []) 
                    in 
                        gsn:strlist
    | otherwise = [gs]
    
extGS :: GannetSymbol -> GannetSymbol
extGS gs = emptyGS{kind=K_X,datatype=datatype gs,name=name gs}    

nwords :: String -> (Integer,Integer)
nwords str = 
    let 
        l = length str
        nbytes = 8   
        m = mod l nbytes
    in
        (toInteger ((div (l-m) nbytes)+(signum m)),toInteger(nbytes-m))
        
splitInWords :: String -> [String] -> [String]
splitInWords str ws 
        | length str > nbytes =
            let
                (w,cs) = splitAt nbytes str 
            in
                splitInWords cs (ws++[w])
        | otherwise = ws++[str]
    where
        nbytes = 8   

extGSstr w = emptyGS{kind=K_X,datatype=T_s,name=GannetTokenB (GannetBuiltinS w)}


-- symbolToWord :: GannetSymbol -> GannetWord

-- For keeping track of variable bindings
-- A variable is fully identified by kind, subtask and name. 
-- We specifically don't include the Datatype 
type GVarName = (GSymbolKind,Integer,GannetToken)
symbolToGVarName :: GannetSymbol -> GVarName
symbolToGVarName gs = (kind gs, subtask gs, name gs)

--------------------------------------------------------------------------------
-- Gannet Packet 
--------------------------------------------------------------------------------
type GannetPacket = (GannetHeader,[GannetSymbol])

data GannetHeader = MkGannetHeader
                    {
                     ptype        :: GPacketType
                    ,prio        :: Int
                    ,redir      :: Int                    
                    ,plength        :: Int
                    ,to            :: GannetLabel
                    ,return_to    :: GannetLabel
                    ,ack_to     :: GannetSymbol
                    ,return_as    :: GannetSymbol
                    }

showGH :: GannetHeader ->  String
showGH s = "\n========\n"++(showGPT (ptype s))++ ":" ++ (show (prio s))++ ":" ++ (show (redir s))++ ":" ++ (show (plength s)) ++ ":" ++ (show (to s)) ++ ":" ++ (show (return_to s)) ++"\n" ++ (show (ack_to s)) ++ "\n" ++ (show (return_as s)) ++ "\n------------\n"
instance Show GannetHeader where show = showGH 

data GPacketType = P_error | P_subtask | P_code | P_reference | P_request | P_data | P_mm | P_lookup | P_address | P_advertise
    deriving (Show,Eq,Ord,Enum)
showGPT :: GPacketType -> String 
showGPT k  
    | numeric = show . fromEnum $ k     
    | otherwise = k'
        where 
            _:_:k' = show k
                            
emptyGH :: GannetHeader 
emptyGH = (MkGannetHeader P_code 0 0 0 (GannetLabelI 0) (GannetLabelI 0) nullGS emptyGS)

--------------------------------------------------------------------------------
-- Gannet Task Description 
--------------------------------------------------------------------------------

type PacketList = Hash.Map GannetSymbol GannetPacket

emptyGPL :: PacketList
emptyGPL = Hash.empty

type PacketRefList = [(GannetSymbol,GannetPacket)]
------
--type TGSymbol = (String, Bool)
--
--data TSymbolTree = TSymbol TGSymbol | TSymbolList ([TSymbolTree],Bool)
--
--showTSVal :: TSymbolTree -> String
--showTSVal (TSymbol (name,q)) = name++":"++ show q
--showTSVal (TSymbolList (contents,q)) = "(" ++ unwordsTSList contents ++ ")" ++ ":" ++ show q
--
--unwordsTSList :: [TSymbolTree] -> String
--unwordsTSList = unwords . map showTSVal
--
--instance Show TSymbolTree where show = showTSVal
------
--------------------------------------------------------------------------------
-- Types to determine return value of a service
--------------------------------------------------------------------------------
 
data GRetVal = GSV GSymbol | GData | GAny deriving (Show,Eq,Ord)
--data GSymbol = GBI | GRef | K_Lref | GLex | GArg | GGen deriving (Show,Eq,Ord)
-- this maps to K_B | K_R | K_Lref | K_L | K_A | K_Unknown
type GSymbol = (GSymbolKind,GDataType) 
-- KLRef is new, and it's for compile-time use only
--type GData=[Word32]

