-- | The tokenizer takes a string (the contents of a .td file) 
-- and parses it into a TokenTree 
module Gannet.Tokenizer (
    tokenize
) where
import Gannet.SBA.Types
import Gannet.TokenParser
import Text.ParserCombinators.Parsec
{- 
TODO: Here in the Tokenizer we should do the syntax checks, 
to make sure that the tree is sound.
-}
tokenize :: String -> TokenTree
tokenize input = tl2tt tl emptyTL []
    where
        tl = stringToTokens input

-- | This is a very simple Parsec lexer which turns a Gannet text file into a list of strings
stringToTokens :: String -> [String]
stringToTokens input = case parse parseTokens "" input of
    Left err -> ["ERROR: " ++ show err]
    Right val -> val

parseTokens :: Parser [String]
parseTokens =  do {
                x <- parseComment <|> parseLParen <|> parseRParen <|> parseQuote <|> parseSymbol <|> parseString;
                spaces;
                do {
                    xs <- parseTokens;
                    return $ [x]++xs
                    }
                <|> return [x]
                }
                

parseLParen :: Parser String
parseLParen = 
    do
        c <- char '('
        return [c]

parseRParen :: Parser String
parseRParen = 
    do
        c <- char ')'
        return [c]
                
parseQuote :: Parser String
parseQuote = 
    do
        c <- char '\''
        return [c]
                
parseSymbol :: Parser String
parseSymbol = many1 (noneOf ")(\'\" \n")

parseComment :: Parser String
parseComment = 
    do
        c <- char ';'
        _ <- spaces
        xs <- many1 (noneOf "\n")
        return $ [c]++xs

parseString :: Parser String
parseString = 
    do
        oq <- char '\"'
        xs <-  many1 (noneOf "\"")
        cq <- char '\"'
        return $ [oq]++xs++[cq]

{-
This code turns a list of strings with paren separators into a tree
using a simple algebraic datatype TokenTree: Token String | TokenList [TokenTree]
So we end up with a tree of lists of strings
-}

{-
How this works:
-we start with an empty TokenTree tree
- we take a token from the tokenlist and keep tokens
- a "(" means we put the current context on the stack, clear the tree and recurse
- a ")" means we take the context from the stack, add the current subtree to the context and recurse
- any other token means we "unwrap" the current tree, add the token to it and wrap it again

The tricky bit is wrapping/unwrapping of the TokenTree: without it, we get
(TokenList (TokenList TokenTree)+TokenTree), whereas we need (TokenList TokenTree+TokenTree)
-}


-- | parse a list of strings into a TokenTree. 
-- TODO: see if use of the State Monad would make this nicer
tl2tt :: [String] -> TokenTree -> [TokenTree] -> TokenTree
tl2tt tokenlist tree stack
    | length tokens == 0    = tree
    | allComments tokens    = tree
    | token=="("            = tl2tt tokens (TokenList ([])) (tree:stack)
    | token==")"             = tl2tt tokens (TokenList (context++[tree])) stack'
--    | token==")" && length tokens > 1           = tl2tt tokens (TokenList (context++[tree])) stack'
--    | token==")" && length tokens == 1 && isComment (head tokens) = tree
    | isComment token        = tl2tt tokens tree stack
    | otherwise         = tl2tt tokens (TokenList (tree'++[Token (parseGToken token tree)])) stack 
    where
        token:tokens = tokenlist
        (TokenList context):stack' = stack        
        (TokenList tree')=tree

isComment :: String -> Bool
isComment x 
    | c == ';' = True
    | otherwise = False
    where
        c:_=x                
    
allComments :: [String] -> Bool
allComments xs = foldl1 (&&) (map isComment xs)
