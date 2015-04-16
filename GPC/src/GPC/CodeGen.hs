{- Generate .td file from GPIR AST -}

module GPC.CodeGen (genCode) where

import GPC.GPIRAST
import Text.PrettyPrint.Leijen

nestLevel = 4 -- |Number of spaces to nest

genCode :: SymbolTree -> String
genCode st = show $ genCode' st False


parSymbol = Symbol $ ConstSymbol False "par"

genCode' :: SymbolTree -> Quoted -> Doc
genCode' (Symbol gSymbol) carryQuote = case gSymbol of
    ConstSymbol quoted str -> text (strQuoted ++ str)
     where strQuoted = if quoted || carryQuote then "'"  else ""

    GOpSymbol (MkOpSymbol quoted thread method) ->
        text $ strQuoted ++ method' method ++ "[" ++ show (snd thread) ++ "]"
     where strQuoted = if quoted || carryQuote then "'" else ""
           method' = foldl1 (\a b -> a ++ "." ++ b)

-- | Generate Source Code for Symbol Tree
genCode' (SymbolList quoted symbolTree) carryQuote = case filter (/= EmptyTree) symbolTree of
    [] -> text ""
    ([x]) -> genCode' x (quoted || carryQuote) -- If tree contains 1 element, carry the quote over
    xs -> if onlyContainsLists xs
                then foldl1 (<+>) (map (`genCode'` (quoted || carryQuote)) xs)
                else parens' (quoted || carryQuote) $
                        foldl1 (<+>) (map (`genCode'` False) xs')

            where xs' = case head xs of
                        (Symbol _) -> xs
                        _ -> parSymbol : xs 
     where
        onlyContainsLists :: [SymbolTree] -> Bool
        onlyContainsLists xs = xs == filter isList xs
        

genCode' EmptyTree _ = text ""

parens' :: Quoted -> Doc -> Doc
parens' q x = nest' nestLevel $ quoteText <> parens x
 where quoteText = text $ if q then "'" else ""

nest' :: Int -> Doc -> Doc
nest' n d = text "" <$> nest n d

isList :: SymbolTree -> Bool
isList (SymbolList _ xs) = length xs > 1
isList _ = False
