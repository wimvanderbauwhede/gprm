-- AST for GPIR

module GPC.GPIRAST (
    SymbolTree(..),
    GannetSymbol(..),
    OpSymbol(MkOpSymbol),
    Quoted
 )
where

data SymbolTree = Symbol GannetSymbol | SymbolList Quoted [SymbolTree] | EmptyTree deriving (Show, Eq)

-- (label LABEL ( ... ))    
data GannetSymbol = 
      ConstSymbol Quoted String
    | GOpSymbol OpSymbol 
--    | LabelKeywordSymbol Quoted String
--    | LabelSymbol Quoted String
    deriving (Show, Eq)

data OpSymbol = MkOpSymbol {
                        quoted  :: Quoted, -- ^ Whether symbol is quoted or not
                        node    :: (String, Integer), -- ^ Node to run on
                        method :: [String] -- ^ Namespace + method
                    } deriving (Eq,Ord, Show)

type Quoted = Bool
