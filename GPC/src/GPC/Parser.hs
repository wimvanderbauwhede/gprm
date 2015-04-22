{- GPC parser -}

module GPC.Parser(parseSource) where

import Control.Applicative hiding ((<|>), many, optional, empty)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import GPC.AST
import GPC.Lexer
import Control.Arrow
import Data.List

{- Operator Tables -}

-- | Need operators to evaluate ordinary expressions and constant expressions
exprOperators pos = operators pos (\n c -> (Prefix (reservedOp n >> return (ExpUnaryOp c))))
                              (\n c -> (Infix  (reservedOp n >> return (ExpBinOp c)) AssocLeft))

-- |Unary operators have higher precedence than binary ones
--
operators pos un bin = unaryOps pos un ++ binaryOps pos bin


-- |Binary operators from highest to lowest precedence
binaryOps :: SrcPos -> (String -> BinOps SrcPos -> Operator s u m a) -> [[Operator s u m a]]
binaryOps pos binary =     
    [[binary "*"  (Mul pos) ,binary "/"  (Div pos), binary "%" (Mod pos)]
     ,[binary "+"  (Add pos), binary "-"  (Sub pos)]
     ,[binary "<<" (ShiftL pos) ,binary ">>" (ShiftR pos)] 
     ,[binary "<"  (Less pos),binary "<=" (LessEq pos)]
     ,[binary ">"  (Greater pos) ,binary ">=" (GreaterEq pos)]
     ,[binary "==" (Equals pos) ,binary "!=" (NEquals pos)]
     ,[binary "&"  (BAnd pos)]
     ,[binary "^"  (BXor pos)]
     ,[binary "|"  (BOr pos)]
     ,[binary "&&" (And pos)]
     ,[binary "||" (Or pos)]
    ]

-- |Unary operators from highest to lowest precedence
unaryOps :: SrcPos -> (String -> UnaryOps SrcPos -> Operator s u m a) -> [[Operator s u m a]]
unaryOps pos unary = [[unary "-" (Neg pos), unary "!" (Not pos), unary "~" (BNot pos)]]

-- | Parse given source file, returns parse error string on
-- | failure otherwise returns the AST for the source
parseSource :: String -> Either String (Program SrcPos)
parseSource = left show . parse program "" . removeCPPPreprocessor


-- | Parse entire source file
program :: Parser (Program SrcPos)
program = Program <$> (whiteSpace *> topLevels) 


-- | Remove C++ preprocessor directives and replace with a blank line 
--   The lexer doesn't support
--   more than one set of characters to mark comments, and doesn't
--   have any way to check the first character in each line
removeCPPPreprocessor :: String -> String
removeCPPPreprocessor s = unlines $ map (\n -> if "#" `isPrefixOf` n then "" else n) (lines s)

-- | Parse top level statements/definitions
topLevels :: Parser [TopLevel SrcPos] 
topLevels = ((:) <$> topLevel <*> topLevels) 
         <|> (eof >> return [])



-- | Parse Top Level definitions
topLevel :: Parser (TopLevel SrcPos)
topLevel = try function
        <|> try (TLAssign <$> assign)
        <|> try (TLObjs <$> objs)
        <|> (TLConstructObjs <$> constructObjs)


-- | Parse C++ Object definitions
objs :: Parser (Objects SrcPos)
objs = do 
--    ns <- sepBy2 parseIdent $ reservedOp "::"
    ns' <- parseIdent
    let
        (Ident pos ns'') = ns'
        ns_ = splitDelim "::" ns''
        ns = map (\n -> (Ident pos n)) ns_    
    var <- parseVar
    _ <- semi
    return $ Objects ns var
--  where sepBy2 seg sep = do
--            x <- seg
--            _ <- sep
--            xs <- sepBy1 seg sep
--            return (x:xs)  


constructObjs :: Parser (ConstructObjs SrcPos)
constructObjs = do 
    var <- parseVar
    reservedOp "="
--    ns <- sepBy1 parseIdent $ reservedOp "::"
    ns' <- parseIdent
    let
        (Ident pos ns'') = ns'
        ns_ = splitDelim "::" ns''
        ns = map (\n -> (Ident pos n)) ns_    
    exprs <- parens $ commaSep expr
    _ <- semi 
    return $ ConstructObjs ns var exprs 


-- | Parse Function definition
function :: Parser (TopLevel SrcPos)
function = Func <$> parseType <*> parseIdentNoNS <*> fArgs <*> block
 where fArgs = parens args

    
-- | Parse Function arguments
args :: Parser [(Type SrcPos, Ident SrcPos)] 
args =  commaSep arg
 where arg :: Parser (Type SrcPos, Ident SrcPos) 
       arg = do 
           aType <- parseType
           aName <- parseIdent
           return (aType, aName)


-- | Parse a block of statements encased in braces
block :: Parser (BlockStmt SrcPos)
block = BlockStmt <$> braces stmts


-- | Parse multiple statements
stmts :: Parser [Stmt SrcPos]
stmts = many stmt


-- | Parse individual statement
stmt :: Parser (Stmt SrcPos)
stmt = try (Return <$> (reserved "return" *> (expr <* semi)))
   <|> try (BStmt <$> block)
   <|> try ifStmt
   <|> try seqBlock
   <|> try parBlock
   <|> try forLoop 
   <|> try (FunCallStmt <$> (funCall <* semi)) 
   <|> try (MethodStmt <$> (methodCall  <* semi))
   <|> try (AssignStmt <$> assign)     


-- | Parse if statement
ifStmt :: Parser (Stmt SrcPos)
ifStmt = try (IfElse <$> parseIf <*> stmt <*> (reserved "else" *> stmt))
     <|>      If     <$> parseIf <*> stmt
 where parseIf = reserved "if" *> parens expr


-- | Parse block to be executed sequentially
seqBlock :: Parser (Stmt SrcPos)
seqBlock = Seq <$> (reserved "seq" *> block)


-- | Parse block to be executed in parallel
parBlock :: Parser (Stmt SrcPos)
parBlock = BStmt <$> (reserved "par" *> block)


-- | Parse Expression
expr :: Parser (Expr SrcPos)
expr = do
    pos <- getPos
    buildExpressionParser (exprOperators pos) expr'
 where expr' :: Parser (Expr SrcPos)
       expr' = try (ExpFunCall <$> funCall) 
           <|> try (ExpMethodCall <$> methodCall)
           <|> try (ExpIdent <$> parseIdent)
           <|> try (ExpLit   <$> literal)
           <|> parens expr

-- | Parse variable assignment
assign :: Parser (Assign SrcPos)
assign = Assign <$> parseType <*> 
                    parseIdent <* parseCh '=' <*> 
                    (expr <* semi)


-- | Parse literal
literal :: Parser (Literal SrcPos)
literal = Ch     <$> getPos <*> ch 
      <|> Str    <$> getPos <*> str   
      <|> Bl     <$> getPos <*> bool 
      <|> Number <$> getPos <*> num 


-- | Parse for loop
forLoop :: Parser (Stmt SrcPos)
forLoop = do
    varName <- reserved "for" *> reservedOp "(" *> reserved "int" *>  parseIdent -- Identifier to use   
    start <- reservedOp "=" *> expr
    stop  <- semi *> expr  -- Stop

    let getPlus =  reservedOp "+=" *> expr
        getMinus = reservedOp "-=" *> (ExpUnaryOp <$> (Neg <$> getPos) <*> expr)
    step  <- semi *> reserved (show varName) *> (try getPlus <|> getMinus) <* reservedOp ")"

    inner <-  block
    return $ ForLoop varName start stop step inner
     

-- | Parse function call
funCall :: Parser (FunCall SrcPos)
funCall = FunCall <$> parseIdent <*> args'
    where args' = parens $ commaSep expr

-- | Parse method call
methodCall :: Parser (MethodCall SrcPos)
methodCall = do
            var <- parseVar
            reservedOp "."
            method <- parseIdent
            args'' <- args'
            return $ MethodCall var method args''
            
    where args' = parens $ commaSep expr


-- | Parse varaible
parseVar :: Parser (Var SrcPos)
parseVar = try (VarArrayElem <$> parseIdent <*> brackets expr) 
       <|> VarIdent <$> parseIdent

-- | Parse identifier
parseIdent :: Parser (Ident SrcPos)
parseIdent = Ident <$> getPos <*> ident

-- | Parse identifier, discard namespace
parseIdentNoNS :: Parser ((Ident SrcPos),(Ident SrcPos))
parseIdentNoNS = do  -- Ident <$> getPos <*> ident
    ns' <- parseIdent
    let
        (Ident pos ns'') = ns'
        _:tn:fn:[] = splitDelim "::" ns'' -- if the name is not "GPRM::<TaskName>::<FunctionName>" then it's an error, need a proper error message here
    return ((Ident pos tn),(Ident pos fn))    


-- | Parse types
-- types can be either one of the basic types (int, bool, char, etc.)
-- or a pointer to a type
parseType :: Parser (Type SrcPos)
parseType = 
    try (reserved "__kernel") *> parseType' True
      <|> parseType' False

parseType' :: Bool -> Parser (Type SrcPos)
parseType' inKernel = do
    baseType <- NormalType <$> getPos <*> pure inKernel <*> typeT
    ptrs <- many getPointer 
    return $ foldr (\ ptr cur -> (ptr cur)) baseType ptrs
 where
    getPointer :: Parser (Type SrcPos -> Type SrcPos)
    getPointer = char '*' >> whiteSpace >> return PointerType
            

-- | Parse number
num :: Parser (Either Integer Double)
num = Right <$> try float
  <|> Left  <$> int

-- | Grab current source position
getPos :: Parser SrcPos
getPos = do
    sp <- getPosition
    return $ SrcPos (sourceLine sp) (sourceColumn sp)

splitDelim :: String -> String -> [String]
splitDelim delim str_ = words (find_delim delim str_ []) 

find_delim :: String -> String -> String -> String    
find_delim delim str_ str' 
    | length str_ == 0 =  str'
    | otherwise = 
        let
            nchars = length delim
            maybe_delim = take nchars str_
        in            
            if maybe_delim == delim 
                then 
                    find_delim delim (drop nchars str_) (str'++" ")
                else 
                    find_delim delim (tail str_) (str' ++[head str_])

