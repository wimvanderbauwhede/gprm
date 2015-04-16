{-# LANGUAGE TemplateHaskell #-}
{- Generate GPIR AST from Type/Scope checked AST
 - Step 1: Generate AST from top level statements
 - Step 2: Interpret code execution from entry function
 -         excluding method calls. -}


module GPC.Interpreter(genGPIR) where 


import           Control.Applicative      hiding (empty, many, optional, (<|>))
import           Control.Error.Util
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Bits
import qualified Data.Map                 as M
import           GPC.TypelessAST
import           GPC.GPIRAST

type ConstVarTable = M.Map Ident Literal
type FunTable = M.Map Ident ([Ident], BlockStmt)
type VarRegTable = M.Map Ident Integer

-- ^ Current State of the Block we are currently in
data CodeGen = CodeGen {
   _funTable    :: FunTable,  -- ^ Store symbol tree for functions
   _constTable  :: ConstVarTable, -- ^ Store constants in scope
   _varId       :: Integer, -- ^ Current variable id for mapping to registers
   _argId       :: Integer, -- ^ Current variable id for mapping to registers
   _varRegTable :: VarRegTable, -- ^ maps variable identifier
   _argRegTable :: VarRegTable, -- ^ maps variable identifier
   _threadCount :: Integer, -- ^ Current thread number to map
   _maxThreads  :: Integer,  -- ^ Maximum number of threads
   _seqBlock    :: Bool, -- ^ Whether or not current block is sequential
   _isReturning :: Bool -- ^ Whether the state of the current block is in a return
}

-- Create lenses to access Block fields easier
makeLenses ''CodeGen

type GenState a = StateT CodeGen (Either String) a

-- | Check if a Top Level statement is an assignment,
--   returns True if it is, False otherwise
isAssign :: TopLevel -> Bool
isAssign tl = case tl of
    TLAssign _ -> True
    _ -> False

-- | Check if a Top Level statement is an Object declaration,
--  returns True if it is, False otherwise
isObject :: TopLevel -> Bool
isObject tl = case tl of
    TLObjs _ -> True
    _ -> False


-- | Check if a Top Level statement is a function declaration,
--  returns True if it is, False otherwise
isFunc :: TopLevel -> Bool
isFunc tl = case tl of
    Func{} -> True
    _ -> False

-- | Given the name of the entry function (currently the filename of the
--   gpc source is used), and a Top Level statement, checks if the 
--   given Top Level statement is the entry function
isNonEntryFunc :: String -> TopLevel -> Bool
isNonEntryFunc eName tl = case tl of
    Func _ fName _ _ -> (fName /= Ident eName) -- && (fName /= Ident ("GPRM::Task::"++eName))
    _ -> False

-- | Update the register table and counter
--   return the next available register
updateRegs :: Ident -> GenState Ident
updateRegs ident = do
    vId <- use varId -- an integer, use is like 'get'
    vRegTable <- use varRegTable -- a map from var name to integer
    let newVarId = vId + 1 -- increment the register counter
    assign varId newVarId -- like 'put'
    assign varRegTable $ M.insert ident newVarId vRegTable
    return $ Ident (show newVarId) -- returns a number as ident
    
-- | Update the register table and counter
--   return the next available register
updateArgs :: Ident -> GenState Ident
updateArgs ident = do
    vId <- use argId
    vArgRegTable <- use argRegTable
    let newArgId = vId + 1
    assign argId newArgId
    assign argRegTable $ M.insert ident vId vArgRegTable
    return $ Ident (show newArgId)    

-- | Obtain the current thread. If the current Block isn't in a seq block
--   then increment the thread counter modulo the max no of threads
--   otherwise keep the thread count the same, as all tasks in the seq block
--   must be run sequentially on the same thread.
getThread :: Bool -> GenState Integer
getThread fc = do
    threadNo <- use threadCount
    maxT <- use maxThreads
    sequential <- use seqBlock
    let 
        newThreadNo = if (sequential || fc) then threadNo else threadNo + 1
        newThreadNo' = newThreadNo `mod` maxT
    assign threadCount newThreadNo'
    return newThreadNo'


-- | Given the Program name, a Type Checked GPC AST, and the number of
-- total threads in the system, attempts to generate a SymbolTree for the
-- Task Description file, returns an error string if unsuccessful
genGPIR :: String -> Program -> Integer -> Either String SymbolTree
genGPIR name (Program tls) threads = case runStateT (genTopLevel name tls) initial of
    Left s -> Left s
    (Right (tl, _)) -> Right tl
 where initial = CodeGen M.empty M.empty 0 0 M.empty M.empty 0 threads False False


-- | Transform all TopLevel statements into a SymbolTree
genTopLevel :: String -> [TopLevel] -> GenState SymbolTree
genTopLevel name tls = do
    genTLAssigns tls
    genFuncs tls
    -- Assignments, Object declarations, and Functions which are non entrt
    -- functions aren't transformed. This is due to top level assignment
    -- statements having to be constants. Non entry functions are
    -- transformed when they are interpreted from the entry function. 
    -- Object declarations are taken care of in config files.
    let tls' = filter (\x -> not (isAssign x || isObject x || isNonEntryFunc name x)) tls
    -- Check program actually has entry function
    when ((length $ filter isFunc tls') == 0) (
        lift $ Left $ "Error, program must contain entry function with name \"" ++ name ++ 
            "\". No function with this name has been defined." 
        )

    tlSymbols <- mapM genTopLevelStmt (filter (not . isFunc) tls')
    mainSymbolTrees <- mapM genTopLevelStmt (filter isFunc tls')

    let seqSymbol = Symbol $ ConstSymbol False "seq"

    if length tlSymbols == 0 
        then return $ SymbolList False mainSymbolTrees
        else return $ SymbolList False $ seqSymbol : (tlSymbols ++ [(SymbolList True mainSymbolTrees)])


-- | Generate all Top Level Assignments
genTLAssigns :: [TopLevel] -> GenState ()
genTLAssigns tls = mapM_ genTLAssign $ filter isAssign tls
  where 
        genTLAssign :: TopLevel -> GenState ()
        genTLAssign (TLAssign (Assign ident expr)) = do 
            expr' <- reduceExpr expr 
            case expr' of

             (ExpLit l) -> do
                    cTable <- use constTable
                    assign constTable $ M.insert ident l cTable
             _ -> lift $ Left $ show expr ++ "Compiler error, in top level assignment code generation"

        genTLAssign _ = lift $ Left "Not top level Assignment statement"


-- | Store all functions, to be evaluated later
-- if called during interpreting
genFuncs :: [TopLevel] -> GenState ()
genFuncs tls = mapM_ genFunc $ filter isFunc tls
  where
        genFunc :: TopLevel -> GenState ()
        genFunc (Func _ name args stmts) = do
            fTable <- use funTable
            assign funTable $ M.insert name (args, stmts) fTable

        genFunc a = lift $ Left $ "Not Function definition" ++ show a


-- | Generate GPIR from Top Level statements
genTopLevelStmt :: TopLevel -> GenState SymbolTree
genTopLevelStmt tl = case tl of
      (TLConstructObjs cObjs) -> genTLConstructObjs cObjs
      (Func _ _ args bStmt) -> genNest False True $ genEntryFunc bStmt args
      _ -> lift $ Left $ "Compiler error, shouldn't contain Top Level " ++
                "Assignments, Object Decls or non Entry functions"


-- | Generate Object Constructors
genTLConstructObjs :: ConstructObjs -> GenState SymbolTree
genTLConstructObjs (ConstructObjs ns var exprs) = do

    curThread <- getThread False
    let constructor = Symbol $ GOpSymbol $
                    MkOpSymbol False ("", curThread) (map (filter (/='"') . show) ns)
    args <- mapM checkConst exprs
    let args' = map (Symbol . ConstSymbol True . show) args

    case var of
        -- TODO work out how to map
        (VarArrayElem _ indexExpr) -> void $ checkConst indexExpr
        (VarIdent _) -> return ()

    return $ SymbolList True (constructor : args')


-- | Interpret Program starting from Entry Function
genEntryFunc :: BlockStmt -> [Ident] ->  GenState SymbolTree
genEntryFunc (BlockStmt stmts) args = do
--    mapM_ updateRegs args -- Create placeholder for entry args
    mapM_ updateArgs args -- Create placeholder for entry args      
    SymbolList False <$> mapM (genStmt False) (takeWhileInclusive (not . isReturn) stmts)


-- | Function to deal with state and nesting
--  we want to give a new context which constant variables are overwritten
--  in the new scope when another is declared, or are erased when a non constant
--  variable of the same name is declared. We also want to keep a running count of
--  the registers used. As well we need to check if a return is made that
--  we exit to the calling function of the current function. We may be
--  several blocks in a function.
genNest :: Bool -> Bool -> GenState a -> GenState a
genNest sequential isTopScopeFun g = do
    curEnv <- get
    case runStateT g curEnv{_seqBlock = sequential}  of
        Left e -> lift $ Left e
        Right (res, afterEnv) -> do
            assign varId (_varId afterEnv)
            assign threadCount (_threadCount afterEnv)
            -- If Returning, and the current scope isn't at the top level
            -- of a function then we continue returning
            when ((_isReturning afterEnv) && (not isTopScopeFun)) (assign isReturning True)
            return res


-- | Generate Statement
genStmt :: Bool -> Stmt ->  GenState SymbolTree
genStmt fc s = do
   
   returning <- use isReturning
   if returning 
     then return EmptyTree 
     else do

       quoted <- use seqBlock
       case s of

        AssignStmt a -> genAssignStmt a quoted fc
        Seq b -> genSeqBlock b quoted -- fc       
        BStmt b -> genParBlock b quoted -- fc
        FunCallStmt f -> genFunCall f quoted -- fc
        MethodStmt m -> genMethodCall m quoted fc
        If expr stmt -> genIfStmt expr stmt quoted fc
        IfElse expr stmt1 stmt2 -> genIfElseStmt expr stmt1 stmt2 quoted fc
        Return expr -> genReturnStmt expr quoted -- fc
        ForLoop ident start stop step stmt' -> genForLoop ident start stop step stmt' quoted -- fc


-- | Generate Assignment Statement
genAssignStmt :: Assign -> Bool -> Bool -> GenState SymbolTree
genAssignStmt (Assign name expr) quoted fc = do
    expr' <- reduceExpr expr
    case expr' of
        -- Values that evaluate to literals will not need to be written
        -- to registers, wherever they are used can be directly
        -- substituted during compliation
        ExpLit l -> do 
            constTable %= M.insert name l
            return EmptyTree
        -- Otherwise store the expression in a register
        -- to be read from when used elsewhere
        _ -> do
            -- If variable non constant, if there exists an entry for a variable in the
            --  above scope of the same name in the constant table, erase it
            constTable %= M.delete name
            curThread <- getThread fc
            let assignSymbol = Symbol $ GOpSymbol $
                        MkOpSymbol False ("", curThread) ["reg", "write"]
            evalExpr <- genExpr expr False
            reg <- updateRegs name
            let regSymbol = Symbol $ ConstSymbol True (filter (/='"') (show reg))
            return $ SymbolList quoted [assignSymbol, regSymbol,  evalExpr]


-- | Generate Sequential Block
genSeqBlock :: BlockStmt -> Bool -> GenState SymbolTree
genSeqBlock = genBlock True "seq"

    
-- | Generate Parallel Block
genParBlock :: BlockStmt -> Bool -> GenState SymbolTree
genParBlock = genBlock False "par"


-- | Generic Block generate
genBlock :: Bool -> String -> BlockStmt -> Bool -> GenState SymbolTree
genBlock isSeq symName (BlockStmt stmts) quoted = do
    let symbol = Symbol $ ConstSymbol False symName
    -- Entering a block, need to nest
    stmt1' <- genNest isSeq False $ genStmt True (head stmts)
    stmts' <- genNest isSeq False $ mapM (genStmt False) (takeWhileInclusive (not . isReturn) (tail stmts))
    if length stmts' > 0
        then return $ SymbolList quoted (symbol : stmt1' : stmts')
        else return $ SymbolList quoted [stmt1']

-- | Evaluate function call, and interpret function
genFunCall :: FunCall -> Bool -> GenState SymbolTree
genFunCall (FunCall name exprs) quoted = do
    exprs' <- mapM reduceExpr exprs
    (BlockStmt stmts) <- genInlineFunc name exprs'
    stmts' <- genNest False True $ mapM (genStmt False) (takeWhileInclusive (not . isReturn) stmts) -- FIXME
    -- WV -- let parSymbol = Symbol $ ConstSymbol False "par"
    --if length stmts' > 1 
    --    then return $ SymbolList quoted (parSymbol : stmts')
    return $ SymbolList quoted stmts'

-- | Generate Method Call
genMethodCall :: MethodCall -> Bool -> Bool -> GenState SymbolTree
genMethodCall (MethodCall var method exprs) quoted fc = do 
    curThread <- getThread fc
    exprs' <- mapM reduceExpr exprs
    objName <-  case var of
          VarIdent (Ident i) -> return i
          VarArrayElem (Ident i) e -> do
            e' <- reduceExpr e 
            case e' of
                (ExpLit (Number (Left n))) -> return $ i ++ show n 
                _ -> lift $ Left $ "Compiler error with array indexing, expected literal integer value got:" 
                        ++ show e'

    let call = Symbol $ GOpSymbol $
                MkOpSymbol False ("", curThread) [objName, filter (/='"') $ show method]
    case length exprs' of
     0 -> return $ SymbolList quoted (call : [] )
     1 -> do
            evalExpr1 <-  genExpr (head exprs') True
            return $ SymbolList quoted (call : evalExpr1 : [])
     _ -> do           
            let expr1:exprs''=exprs'               
            evalExpr1 <-  genExpr expr1 True
            evalExprs <- mapM (\e -> genExpr e False) exprs''
            return $ SymbolList quoted (call : evalExpr1 : evalExprs)
--    else 
--     if length exprs' == 1 
--      then
--     error $ "Expression too short: "++(show exprs')

-- | Generate If Statement
genIfStmt :: Expr -> Stmt -> Bool -> Bool -> GenState SymbolTree
genIfStmt expr stmt quoted fc = do
    expr' <- reduceExpr expr
    case expr' of
        -- If Expression is a literal boolean we can evaluate the
        -- branch at compile time
     (ExpLit (Bl b)) -> 
        if b 
        then (genStmt False) stmt                   
        else return EmptyTree

     -- Otherwise generate code to evaluate at runtime
     _ -> do
        curThread <- getThread fc
        let ifSymbol = Symbol $ GOpSymbol $
             MkOpSymbol False ("", curThread) ["if"]

        cond <- genExpr expr' True
        evalStmt <- (genStmt False) stmt
        -- GPRM supports only if-then-else style, 
        --  generate dummy statement to fill in the "else"
        let dummyStmt = Symbol $ ConstSymbol True "0"
        return $ SymbolList quoted [ifSymbol, cond, evalStmt, dummyStmt]

-- | Generate If Else statement
genIfElseStmt :: Expr -> Stmt -> Stmt -> Bool -> Bool-> GenState SymbolTree
genIfElseStmt expr stmt1 stmt2 quoted fc = do
    expr' <- reduceExpr expr
    case expr' of
        --Again like the If expression see if we can evaluate
        -- which branch to take during compile time
        (ExpLit (Bl b)) -> 
            if b 
              then (genStmt False) stmt1
              else (genStmt False) stmt2

        _ -> do
            curThread <- getThread fc
            let ifSymbol = Symbol $ GOpSymbol $
                    MkOpSymbol False ("", curThread) ["if"]
            cond <- genExpr expr' True
            evalStmt1 <- (genStmt False) stmt1
            evalStmt2 <- (genStmt False) stmt2
            return $ SymbolList quoted [ifSymbol, cond, evalStmt1, evalStmt2]


-- | Generate Return Stmt
genReturnStmt :: Expr -> Bool -> GenState SymbolTree
genReturnStmt expr quoted = do
    expr' <- reduceExpr expr
    evalExpr <- genExpr expr' True
    assign isReturning True
    return $ SymbolList quoted [evalExpr]

-- | generate For Loop by entirely unrolling it, compiler cannot currently
--   deal with infinite loops
genForLoop :: Ident -> Expr -> Expr -> Expr -> BlockStmt -> Bool -> GenState SymbolTree
genForLoop ident start stop step (BlockStmt stmts) quoted = do
   start' <- getInt =<< reduceExpr start -- Get exact start value
   stop'  <- reduceExpr stop -- Get stop expr but don't evaluate it 
   step'  <- getInt =<< reduceExpr step -- Get exact stop value

  
   -- Generate an infinite loop of unrolled statements, iterating through
   let unrolledStmts = concatMap (\i ->
                            map (genInlineStmt [(ident, ExpLit (Number (Left i)))]) stmts)
                                $ iterate (+ step') start'

   -- Obtain number of statements until end condition met
   noStmts <- (* length stmts) `fmap` lengthInBounds (iterate (+step') start') ident stop'
   
   unrolledStmts' <- (genStmt False) $ BStmt $ BlockStmt
                    (takeWhileInclusive (not . isReturn) $ take noStmts unrolledStmts)
   return $ SymbolList quoted [unrolledStmts']

     where isInBounds :: Integer -> Ident -> Expr -> GenState Bool
           isInBounds val ident' stop' = getBool =<< (reduceExpr $
                    replaceExprIdent ident' (ExpLit (Number (Left val))) stop')

           lengthInBounds :: [Integer] -> Ident -> Expr -> GenState Int
           lengthInBounds (x:xs) ident' stop' = do
                inBounds <- isInBounds x ident' stop'
                if inBounds
                    then do
                        next <- lengthInBounds xs ident' stop'
                        return $ next + 1
                    else return 0

           lengthInBounds [] _ _ = return 0
           
           getInt :: Expr -> GenState Integer
           getInt (ExpLit (Number (Left i))) = return i
           getInt er = lift $ Left $ "Compiler error, expected integer value from expression, got:" ++ 
                        show er

           getBool :: Expr -> GenState Bool
           getBool (ExpLit (Bl b)) = return b
           getBool er = lift $ Left $ "Compiler error, expected boolean value from expression, got:" ++ 
                        show er


-- | Generate an Expression
genExpr :: Expr -> Bool -> GenState SymbolTree
genExpr e fc = do 
    expr <- reduceExpr e
    case expr of

         ExpBinOp binOp lExpr rExpr -> genBinExpr binOp lExpr rExpr fc
         ExpUnaryOp unOp expr' -> genUnaryExpr unOp expr' fc
         ExpFunCall f -> genFunCall f False -- fc
         ExpMethodCall m -> genMethodCall m False fc 
         ExpIdent ident -> genIdentExpr ident fc
         ExpLit lit -> return $ Symbol $ ConstSymbol True (show lit)


genBinExpr :: BinOps -> Expr -> Expr -> Bool -> GenState SymbolTree
genBinExpr bOp lExpr rExpr fc = do
    curThread <- getThread fc
    let method = case bOp of
            Add -> "+"
            Sub -> "-"
            Mul -> "*"
            Div -> "/"
            And -> "&&"
            Or -> "||"
            Mod -> "%"
            Less -> "<"
            LessEq -> "<="
            Greater -> ">"
            GreaterEq -> ">="
            Equals -> "=="
            NEquals -> "!="
            ShiftL -> "<<"
            ShiftR -> ">>"
            BAnd -> "&"
            BXor -> "^"
            BOr -> "|"

    let binSymbol = Symbol $ GOpSymbol $
                MkOpSymbol False ("", curThread) [method]

    lExpr' <- genExpr lExpr fc
    rExpr' <- genExpr rExpr fc
    return $ SymbolList False [binSymbol, lExpr', rExpr']


genUnaryExpr :: UnaryOps -> Expr -> Bool -> GenState SymbolTree
genUnaryExpr unOp expr fc = do
    curThread <- getThread fc
    let method = case unOp of
                Not -> "!"
                Neg -> "-"
                BNot -> "~"

    let unSymbol = Symbol $ GOpSymbol $
                MkOpSymbol False ("", curThread) [method]
    expr' <- genExpr expr fc
    return $ SymbolList False [unSymbol, expr']

{-
genIdentExpr :: Ident -> GenState SymbolTree
genIdentExpr ident = do
    curThread <- getThread
    regTable <- use varRegTable
    reg <- lift $ note regNotFound $ M.lookup ident regTable
    let regSymbol = Symbol $ GOpSymbol $
                    MkOpSymbol False ("", curThread) ["reg", "read"]
    return $ SymbolList False  [regSymbol, Symbol $ ConstSymbol True (show reg)]
  
  where regNotFound = "Compiler error, register for ident " ++ show ident ++ "not found"
-}

genIdentExpr :: Ident -> Bool -> GenState SymbolTree
genIdentExpr ident fc = do
    curThread <- getThread fc
    regTable <- use varRegTable
    argTable <- use argRegTable    
    reg <- lift $ case M.lookup ident regTable of
        Just r -> note regNotFound (Just r)
        Nothing -> case M.lookup ident argTable of
            Just a -> note regNotFound (Just a)
            Nothing -> note regNotFound Nothing
    let oper = case M.lookup ident regTable of
                Just _ -> "reg"
                Nothing -> case M.lookup ident argTable of
                            Just _ -> "arg"
                            Nothing -> "NONE"            
    let regSymbol = Symbol $ GOpSymbol $
                    MkOpSymbol False ("", curThread) [oper, "read"]
    return $ SymbolList False  [regSymbol, Symbol $ ConstSymbol True (show reg)]
  
  where regNotFound = "Compiler error, register for ident " ++ show ident ++ "not found"
            
-- | Generate Inline Function by replacing all identifiers
-- | in scope with supplied argument expressions
genInlineFunc :: Ident -> [Expr] -> GenState BlockStmt
genInlineFunc name args = do
    fTable <- use funTable
    case M.lookup name fTable of
        Just (idents, stmts) -> do
            let (BStmt stmts') = genInlineStmt (zip idents args) (BStmt stmts)
            return stmts'

        Nothing -> lift $ Left "Compiler error genInlineFunc"


-- | Replace all instance of given identifiers in a statement with
--   a given expression
genInlineStmt :: [(Ident, Expr)] -> Stmt -> Stmt
genInlineStmt exprs stmt = case stmt of

    AssignStmt (Assign name expr) ->
        AssignStmt (Assign name $ replaceExprIdents exprs expr)

    FunCallStmt (FunCall name args) ->
        FunCallStmt (FunCall name $ map (replaceExprIdents exprs) args)

    MethodStmt (MethodCall var method args) ->
        MethodStmt (MethodCall (newVar var) method $ map (replaceExprIdents exprs) args)

    If expr stmt' ->
        If (replaceExprIdents exprs expr) (genInlineStmt exprs stmt')

    IfElse expr stmt1 stmt2 ->
        IfElse (replaceExprIdents exprs expr) (genInlineStmt exprs stmt1)
                                              (genInlineStmt exprs stmt2)

    Return expr -> Return $ replaceExprIdents exprs expr

    ForLoop name start stop step (BlockStmt stmts) ->
        ForLoop name (replaceExprIdents exprs start)
                     (replaceExprIdents exprs stop)
                     (replaceExprIdents exprs step)
                     (BlockStmt $ replaceIdents stmts)

    Seq (BlockStmt stmts) -> Seq $ BlockStmt $ replaceIdents stmts

    BStmt (BlockStmt stmts) -> BStmt $ BlockStmt $ replaceIdents stmts

  where
    -- Once an identifier goes out of scope we don't replace it
    -- map up until the identifier is out of scope, and then add on the rest of
    -- the unmapped statements to the block as they won't need any substitution
    replaceIdents :: [Stmt] -> [Stmt]
    replaceIdents stmts = mappedVals ++ drop (length mappedVals) stmts
      where mappedVals = incMapWhile inScope (genInlineStmt exprs) stmts
    inScope (AssignStmt (Assign name _)) = name `notElem` idents
    inScope _ = True
    idents = map fst exprs
    newVar v = case v of
          VarArrayElem i e -> VarArrayElem i (replaceExprIdents exprs e)
          _ -> v 


-- | Replace instances of identities in an expression
--   with another expression 
replaceExprIdents :: [(Ident, Expr)] -> Expr -> Expr
replaceExprIdents replaceExprs givenExpr =
    foldl (\gExpr (ident, rExpr) -> replaceExprIdent ident rExpr gExpr) givenExpr replaceExprs

-- | Replace all instances of an identity in an expression
-- | with a given sub-expression 
replaceExprIdent :: Ident -> Expr -> Expr -> Expr
replaceExprIdent ident replaceExpr givenExpr = case givenExpr of

    ExpBinOp binOp lExpr rExpr ->
        ExpBinOp binOp (replaceExprIdent ident replaceExpr lExpr)
                       (replaceExprIdent ident replaceExpr rExpr)

    ExpUnaryOp unOp expr -> ExpUnaryOp unOp (replaceExprIdent ident replaceExpr expr)

    ExpFunCall (FunCall name exprs) ->
        ExpFunCall (FunCall name $
            map (replaceExprIdent ident replaceExpr) exprs)

    ExpMethodCall (MethodCall cName method exprs) ->
        ExpMethodCall (MethodCall cName method $
            map (replaceExprIdent ident replaceExpr) exprs)

    ExpIdent expId -> if expId == ident then replaceExpr else ExpIdent expId

    ExpLit lit -> ExpLit lit


-- | inclusive MapWhile function, returns results which satisfy a condition
incMapWhile :: (b -> Bool) -> (a -> b) -> [a] -> [b]
incMapWhile cond f (x:xs) = if cond res then res : incMapWhile cond f xs else [res]
 where res = f x
incMapWhile _ _ [] = []


-- | Checks if given Expression is a Literal value
checkConst :: Expr -> GenState Literal
checkConst expr = case expr of
    (ExpLit l) -> return l
    _ -> lift $ Left "Expected constant expression"


-- | Attempts to reduce an expression as much as possible
-- | Returns an error string if evaluated expression
-- | is invalid or an identifier is not present in the given table
-- | otherwise returns the reduced expression
reduceExpr :: Expr -> GenState Expr
reduceExpr expr = do 
    cTable <- use constTable
    case expr of
        (ExpBinOp b e1 e2) -> do
            re1 <- reduceExpr e1
            re2 <- reduceExpr e2
            lift $ evaluateBinExpr b re1 re2

        (ExpUnaryOp u e) -> do
            reducedExpr <- reduceExpr e
            lift $ evaluateUnExpr u reducedExpr

        (ExpFunCall (FunCall s exps)) -> do
             rexps <- mapM reduceExpr exps
             return $ ExpFunCall (FunCall s rexps)

        (ExpMethodCall (MethodCall obj method args)) -> do
            rexps <- mapM reduceExpr args
            return $ ExpMethodCall (MethodCall obj method rexps)

        (ExpIdent i) -> return $ case M.lookup i cTable of
                            Just l -> ExpLit l
                            Nothing -> ExpIdent i

        (ExpLit l) -> return $ ExpLit l


-- | Attempts to evaluate a constant binary expression
evaluateBinExpr :: BinOps -> Expr -> Expr -> Either String Expr

-- | Binary operations with literals
evaluateBinExpr b (ExpLit l1) (ExpLit l2) = 
    case (b, l2) of
        -- Check for division by 0
        (Div, Number (Left 0)) -> divBy0Err 
        (Div, Number (Right 0.0)) -> divBy0Err
        _ -> binOpTable b l1 l2
  where divBy0Err = Left "Error, attempted to divide by 0"

evaluateBinExpr  b e1 e2 = return $ ExpBinOp b e1 e2
 

-- | Obtain binary operation to use with literal values
binOpTable :: BinOps -> Literal -> Literal -> Either String Expr
binOpTable b = case b of
    Add -> performBinNumOp (+) 
    Sub -> performBinNumOp (-) 
    Mul -> performBinNumOp (*)

    Div -> performDivision 

    Mod -> performBinIntOp mod
    BAnd -> performBinIntOp (.&.) 
    BOr -> performBinIntOp (.|.) 
    BXor -> performBinIntOp xor
    ShiftL -> performBinIntOp (\x y ->  shift x $ fromIntegral y)
    ShiftR -> performBinIntOp (\x y ->  shift x $ fromIntegral (-y)) 

    Less -> performBinCompareOp (<) 
    LessEq -> performBinCompareOp (<=) 
    Greater -> performBinCompareOp (>) 
    GreaterEq -> performBinCompareOp (>=) 
    Equals -> performBinCompareOp (==) 
    NEquals -> performBinCompareOp (/=) 

    And -> performBinBoolOp (&&) 
    Or -> performBinBoolOp (||) 


performBinNumOp :: (forall a. Num a => (a -> a -> a))  -> Literal -> Literal -> Either String Expr
performBinNumOp operation (Number (Left n1)) (Number (Left n2)) =
    Right $ ExpLit $ Number $ Left $ n1 `operation` n2

performBinNumOp operation (Number (Right n1))(Number (Right n2)) =
    Right $ ExpLit $ Number $ Right $ n1 `operation` n2

performBinNumOp _ _ _ = Left "Error expected a numeric value"


performDivision :: Literal -> Literal -> Either String Expr
performDivision (Number (Left n1)) (Number (Left n2)) = Right $ ExpLit $ Number (Left $ n1 `div` n2)
performDivision (Number (Right n1)) (Number (Right n2)) = Right $ ExpLit $ Number (Right $ n1 / n2)
performDivision mn1 mn2 = Left ("Error, expected to divide either 2 integers or 2 doubles: "++(show mn1)++"/"++(show mn2))

performBinIntOp :: (Integer -> Integer -> Integer)  -> Literal -> Literal -> Either String Expr
performBinIntOp operation (Number (Left n1)) (Number (Left n2)) =
    Right $ ExpLit $ Number $ Left $ n1 `operation` n2
performBinIntOp _ _ _ = Left "Error expected integer types"


performBinCompareOp :: (Literal -> Literal -> Bool) -> Literal -> Literal -> Either String Expr
performBinCompareOp operation l1 l2 =
    Right $ ExpLit $ Bl $ l1 `operation` l2


performBinBoolOp :: (Bool -> Bool -> Bool) -> Literal -> Literal -> Either String Expr
performBinBoolOp operation (Bl b1) (Bl b2) =
    Right $ ExpLit $ Bl $ b1 `operation` b2
performBinBoolOp _ _ _ = Left "Error expected boolean values"


-- |Attempts to evaluate a constant unary expression, check the types as
-- |well
evaluateUnExpr :: UnaryOps -> Expr -> Either String Expr
evaluateUnExpr unOp (ExpLit l) = unOpTable unOp l
evaluateUnExpr u e = Right (ExpUnaryOp u e)

-- | Function table of Unary operations on literals
unOpTable u = case u of
    Not -> performUnNotOp
    Neg -> performUnNegOp
    BNot -> performUnBNotOp

-- | Perform Boolean NOT operation on literal value
performUnNotOp ::  Literal -> Either String Expr
performUnNotOp (Bl b1) = Right $ ExpLit $ Bl $ not b1
performUnNotOp _ = Left "Error expected boolean value"

-- | Perform Negation operation on literal value
performUnNegOp :: Literal -> Either String Expr
performUnNegOp (Number (Left i)) =  Right $ ExpLit $ Number $ Left  $ negate i
performUnNegOp (Number (Right i)) = Right  $ ExpLit $ Number $ Right $ negate i
performUnNegOp _ = Left "Error expected numeric type"

-- | Perform Bitwise NOT operation on literal value
performUnBNotOp :: Literal -> Either String Expr
performUnBNotOp (Number (Left i)) = Right $ ExpLit $ Number $ Left $ complement i
performUnBNotOp _ = Left "Error expected integer value"



takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
