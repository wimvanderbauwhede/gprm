{- GPC lexer -}

{-# LANGUAGE NoMonomorphismRestriction #-}

module GPC.Lexer
    ( ident
    , reserved
    , reservedOp
    , parens
    , int
    , ch
    , str
    , float
    , bool
    , semi
    , whiteSpace
    , braces
    , typeT
    , commaSep
    , parseCh
    , brackets
    ) where

import Control.Applicative hiding (empty, many,optional, (<|>))
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

reservedTypes = ["int", "double", "bool","void"] :: [String]
otherReserved = ["if", "else", "true", "false", "seq", "par","return", "for"] :: [String]

-- Define the tokens used in our language
languageDef = emptyDef {
       Token.commentStart  = "/*"
     , Token.commentEnd    = "*/"
     , Token.commentLine   = "//"
     , Token.nestedComments = True
     , Token.identStart    = letter 
     , Token.identLetter   = alphaNum <|> oneOf "'_:" -- WV: '::' is now a part of the identifier
     , Token.reservedNames =   otherReserved ++ reservedTypes
                               
     , Token.caseSensitive = True
     }

lexer = Token.makeTokenParser languageDef

-- Setup parsers for language
ident = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer
--comma = Token.comma lexer
braces = Token.braces lexer
commaSep = Token.commaSep lexer
--semiSep = Token.semiSep lexer
brackets = Token.brackets lexer

-- Parse specific character
parseCh c = reserved [c]
-- Parse specific string
parseStr = reserved

-- Setup parsers for literal values
ch = Token.charLiteral lexer
int = Token.hexadecimal lexer <|> Token.octal lexer <|> Token.integer lexer
float = Token.float lexer
str = Token.stringLiteral lexer
bool = parseStr "true" *> pure True
   <|> parseStr "false" *> pure False

-- |When we need to use built in types
typeT = Token.identifier typeLexer
 where typeLexer = Token.makeTokenParser languageDef
                   {Token.reservedNames = otherReserved}
