{-# LANGUAGE TemplateHaskell #-}
{- Check types and scope of identifiers -}
module GPC.TypeScopeChecker(
    getTypeExpr, runTypeChecker) where


import           Control.Applicative hiding (empty, many, optional, (<|>))
import           Control.Error.Util
import           Control.Lens
import           Control.Monad.State.Lazy

import           Data.Tuple
import qualified Data.Map as M

import           GPC.AST
import           GPC.Errors

type VarTable = M.Map (Ident SrcPos) (Type SrcPos)
type FunTable = M.Map (Ident SrcPos) (Type SrcPos, [Type SrcPos])
type ObjectTable = M.Map (Ident SrcPos) (Objects SrcPos)


data MainBlock = MainBlock {
    _tlFuncDefs      :: FunTable, -- ^ Function Definitions
    _tlVarTypes      :: VarTable,  -- ^ Top Level Constant variable types
    _objects         :: ObjectTable  -- ^ Table of current Kernel objects declared 
} deriving (Show)


data CodeBlock = CodeBlock {
    _currentFun :: Ident SrcPos, -- ^ Name of Function block is in
    _funcDefs   :: FunTable, -- ^ Function names and return/argument types
    _prevVars   :: VarTable, -- ^ Identifiers visible in current scope with types
    _curVars    :: VarTable  -- ^ Identifiers declared in current scope
} deriving (Show)


-- Create lenses to access Block fields easier
makeLenses ''MainBlock
makeLenses ''CodeBlock


-- Monad Transformer combining State with Either
-- when doing type checking if a failure occurs
-- we can return an error String
type GenericBlockState a b = StateT a TypeErrorMonad b
type CodeState a = GenericBlockState MainBlock a 
type BlockState a = GenericBlockState CodeBlock a

raiseError :: TypeScopeError -> GenericBlockState a b
raiseError = lift . Left

-- | Perform Type/Scope checking, and simple expression reduction
-- | Returns either an error message or the Reduced GPC AST
runTypeChecker :: Program SrcPos -> Either TypeScopeError ()
runTypeChecker (Program tls) = case runStateT (evalTLStmts tls) initialBlock of
 Left s -> Left s
 _ -> Right ()
 where initialBlock = MainBlock M.empty M.empty M.empty


-- | Type Check all top level statements
evalTLStmts :: [TopLevel SrcPos] -> CodeState ()
evalTLStmts = mapM_ evalTLStmt


-- | Type check a given top level statement
evalTLStmt :: TopLevel SrcPos -> CodeState ()
evalTLStmt tl = case tl of
    (TLAssign a) -> evalTLAssign a
    (Func gType (_,ident) args stmts) -> evalFunc gType ident args stmts
    (TLObjs objs) -> evalObjs objs
    (TLConstructObjs cObjs) -> evalConstruct cObjs

-- | Type check object initializations
evalConstruct :: ConstructObjs SrcPos -> CodeState ()
evalConstruct (ConstructObjs ns var exprs) = do
    tVars <- use tlVarTypes
    objs <- use objects
    _ <- lift $ mapM (getTypeExpr tVars M.empty) exprs

    case var of
        (VarIdent ident) -> do
            v <- checkNameSpace ident objs
            case v of
                (VarIdent _) -> return ()
                (VarArrayElem i _) -> raiseError (IndexingNotArray i)

        (VarArrayElem ident@(Ident sp _) expr) -> do -- Check indexed expression
            
            v <- checkNameSpace ident objs
      
            -- Check index is an integer value
            exprType <- lift $ getTypeExpr tVars M.empty expr
            checkType (intTypePos notKernel sp) exprType

            case v of 
                (VarIdent i) -> raiseError (AssignArray i) 
                (VarArrayElem (Ident sp' _) expr') -> do
                   
                    exprType' <- lift $ getTypeExpr tVars M.empty expr'
                    checkType (intTypePos notKernel sp') exprType'
    
  where             
    checkNameSpace i objs = do {
        (Objects objNs oVar) <- lift $ note (NotInScope i) (M.lookup i objs);
        if objNs /= init ns then
             raiseError (WrongNamespace ns objNs)
        else if last ns /= last (init ns) 
            then raiseError (ExpectedConstructor (init ns ++ [last ns]) ns)
            else return oVar
    }



-- | Type check object declarations
evalObjs :: Objects SrcPos -> CodeState ()
evalObjs objs@(Objects _ var) = do
    
    tVars <- use tlVarTypes
    case var of
        -- Single Object, check identifier isn't already in scope
        (VarIdent ident@(Ident sp _)) -> do
            checkMultipleInstance ident tVars
            objects %= M.insert ident objs 
            assign tlVarTypes $ M.insert ident (NormalType sp inKernel "object") tVars

        -- Static Array of Objects, check type of array size
        (VarArrayElem ident@(Ident sp _) expr) -> do
            checkMultipleInstance ident tVars
            exprType <- lift $ getTypeExpr tVars M.empty expr
            checkType (intTypePos notKernel sp) exprType
            objects %= M.insert ident objs 
            assign tlVarTypes $ M.insert ident (NormalType sp inKernel "objArray") tVars


-- | Type Check top level assignment
evalTLAssign :: Assign SrcPos -> CodeState ()
evalTLAssign (Assign typeG ident expr) = do
    tVars <- use tlVarTypes
    exprType <- lift $ getTypeExpr tVars M.empty expr

    -- Check Types match and Variable is Single instance
    checkType typeG exprType
    checkMultipleInstance ident tVars
    assign tlVarTypes $ M.insert ident typeG tVars


-- | Type check Function
evalFunc :: Type SrcPos -> Ident SrcPos -> [(Type SrcPos, Ident SrcPos)] 
                        -> BlockStmt SrcPos -> CodeState ()
evalFunc typeG ident args (BlockStmt stmts) = do
    fTable <- use tlFuncDefs
    varTypes <- use tlVarTypes

    -- Check function isn't already defined
    if ident `M.notMember` fTable then
        -- Check argument identifiers only occur once each
        case hasDuplicate (map snd args) of
            Just argId -> raiseError (DuplicateArgNames ident argId)
            Nothing -> do

             -- Create a new variable scope based on the identifiers
             -- in the function definition
             let args' = M.fromList $ map swap args

             -- Add current function to the function scope
             let newFTable = M.insert ident (typeG, map fst args) fTable
--             let 
--                (Ident pos fname) = ident
--                fqn = splitDelim "::" fname
--                ident' = Ident pos (last fqn)               
--             let newFTable = M.insert ident' (typeG, map fst args) newFTable'

             -- Type Check function
             let newBlock = CodeBlock ident newFTable varTypes args'
             lift $ runBlockCheck stmts newBlock

             -- Update Global function scope, and return reduced function
             -- AST
             assign tlFuncDefs newFTable

    else raiseError (MultipleFunctionDecl ident)
  where hasDuplicate (x:xs) = if x `elem` xs then Just x else hasDuplicate xs
        hasDuplicate _ = Nothing

-- | Run Type Checker on new code block
runBlockCheck :: [Stmt SrcPos] -> CodeBlock -> Either TypeScopeError ()
runBlockCheck stmts cb =  case runStateT (evalStmts stmts) cb of
    Left s -> Left s
    _ -> Right ()


-- | Type Check all statements in the current scope
evalStmts :: [Stmt SrcPos] -> BlockState ()
evalStmts = mapM_ evalStmt


-- | Type check given statement
evalStmt :: Stmt SrcPos -> BlockState ()
evalStmt stmt = case stmt of
   (AssignStmt a) -> checkAssign a
   (If expr stmt') -> checkIf expr stmt'
   (IfElse expr stmt1 stmt2) -> checkIfElse expr stmt1 stmt2
   (Seq blockStmt) -> checkBlock blockStmt M.empty
   (BStmt blockStmt) -> checkBlock blockStmt M.empty
   (Return expr) -> checkReturn expr
   (ForLoop ident expr1 expr2 expr3 stmts) -> checkForLoop ident expr1 expr2 expr3 stmts
   (MethodStmt method) -> checkMethodCall method
   (FunCallStmt fc) -> checkFunCall fc


-- | Type check Function Call args
checkFunCall :: FunCall SrcPos -> BlockState ()
checkFunCall f@(FunCall _ _) = do    
    fTable <- use funcDefs
    oldVtable <- use prevVars
    vTable <- use curVars
    let scopeVars = vTable `M.union` oldVtable
    _ <- lift $ getTypeExpr scopeVars fTable (ExpFunCall f)
    return ()


-- |Type Check Assignment Statement
checkAssign :: Assign SrcPos -> BlockState ()
checkAssign (Assign gType ident expr) = do
    ftable <- use funcDefs
    oldVtable <- use prevVars
    vTable <- use curVars
    let scopeVars = vTable `M.union` oldVtable -- Gives all visible identifiers
    
    -- Check new identifier hasn't already been defined in the current
    -- scope
    redefineCheck vTable

    exprType <- lift $ getTypeExpr scopeVars ftable expr

    -- If expression is a method call, we implicitly lift the type
    -- of the entire expression into the GPRM::Kernel
    if isMethodCall expr then do
        let gType' = castToKernel gType
        assign curVars $ M.insert ident gType' vTable

    else do 
        checkType gType exprType
        -- Update Var table with new variable
        assign curVars   $ M.insert ident gType vTable

 where
    redefineCheck vTable = when (ident `M.member` vTable) $ raiseError (Redefine ident)

    isMethodCall e = case e of
        (ExpMethodCall (MethodCall{})) -> True
        _ -> False


-- |Type Check If Statement
checkIf :: Expr SrcPos -> Stmt SrcPos -> BlockState ()
checkIf expr stmt = do
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars
    exprType <- lift $ getTypeExpr scopeVars fTable expr
    checkType exprType (boolType notKernel)
    evalStmt stmt

-- |Type Check If - Else Statement
checkIfElse :: Expr SrcPos -> Stmt SrcPos -> Stmt SrcPos -> BlockState ()
checkIfElse expr thenStmt elseStmt = do
    _ <- checkIf expr thenStmt
    evalStmt elseStmt


-- | Type check for loop
checkForLoop :: Ident SrcPos -> Expr SrcPos -> Expr SrcPos 
                             -> Expr SrcPos -> BlockStmt SrcPos 
                             -> BlockState ()
checkForLoop ident@(Ident sp _) startExpr stopExpr stepExpr blockStmt = do
    fTable <- use funcDefs
    cVars <- use curVars
    --  Need to temporarily add loop variable to
    --  current variable list, as it's not in scope yet
    --  to type check
    assign curVars $ M.insert ident (intTypePos notKernel sp) cVars 
    scopeVars <- M.union <$> use curVars <*> use prevVars

    -- Check types of each expression are all integers
    -- Then type check the for block
    let exprs = [startExpr, stopExpr, stepExpr]
    types <- lift $ mapM (getTypeExpr scopeVars fTable) exprs
    checkType (types !! 0) $ intType notKernel 
    checkType (types !! 1) $ boolType notKernel
    checkType (types !! 2) $ intType notKernel
    assign curVars cVars

    checkBlock blockStmt (M.singleton ident (intTypePos notKernel sp))
    return ()


-- | Type check inner block, add to current list of inner blocks
checkBlock :: BlockStmt SrcPos -> VarTable -> BlockState ()
checkBlock (BlockStmt stmts) innerTable = do
    fName <- use currentFun
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars

    -- Create and type check new inner block, and add to current
    -- list of inner blocks if successful
    let newBlock = CodeBlock fName fTable scopeVars innerTable
    lift $ runBlockCheck stmts newBlock


-- | Type check return stmt
checkReturn :: Expr SrcPos -> BlockState ()
checkReturn expr = do
    fName <- use currentFun
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars
    
    (retType, _) <- lift $ note (NotInScope fName) $ M.lookup fName fTable
    exprType <- lift $ getTypeExpr scopeVars fTable expr

    checkType retType exprType
    return ()


checkMethodCall :: MethodCall SrcPos -> BlockState ()
checkMethodCall m = do
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars
    _ <- lift $ getTypeExpr scopeVars fTable (ExpMethodCall m)
    return ()


-- | Checks that 2 given types match
checkType :: (Show a) =>  Type SrcPos -> Type a -> GenericBlockState b ()
checkType expected actual =
    unless (stripAnnType expected == stripAnnType actual)
        $ raiseError (TypeMismatch expected actual) 


checkType' :: (Show a) => Type SrcPos -> Type a -> Either TypeScopeError ()
checkType' expected actual =
    unless (stripAnnType expected == stripAnnType actual) 
        $ Left (TypeMismatch expected actual) 


-- | Given an identity and var table, if the identity is already
-- part ofr the variable table, then returns a multiple instance error
checkMultipleInstance :: Ident SrcPos -> VarTable -> GenericBlockState a ()
checkMultipleInstance ident vTable = 
    when (ident `M.member` vTable) $ raiseError (NotInScope ident)  

-- | Obtain Type of Expression, returns error message
-- | if types arn't consistent, or identifiers arn't in scope
getTypeExpr :: VarTable -> FunTable -> Expr SrcPos -> Either TypeScopeError (Type SrcPos)
getTypeExpr vtable ftable expr = case expr of
    (ExpBinOp b e1 e2) -> getTypeBinOp vtable ftable b e1 e2
    (ExpUnaryOp u e) -> getTypeUnOp vtable ftable u e
    (ExpFunCall (FunCall s exps)) -> do
        argTypes <- mapM (getTypeExpr vtable ftable) exps
        (retT, ts) <- note (NotInScope s) (M.lookup s ftable)
        if length argTypes /= length ts
            then Left (WrongNoArgs s (length ts) (length argTypes))
            else do
                checkArgTypes (zip ts argTypes)
                return retT

    (ExpMethodCall (MethodCall var (Ident sp _) args)) -> do
        _ <- mapM (getTypeExpr vtable ftable) args
        case var of
            (VarIdent i) -> do
                gType <- findType i vtable
                checkType' gType objectType            
                return $ NormalType sp inKernel "object"

            -- Check we're accessing an element of an array of objects,
            -- and that the element is an integer type
            (VarArrayElem i el) -> do
                gType <- findTypeA i vtable
                elemType <- getTypeExpr vtable ftable el
                checkType' elemType (intType False)
                checkType'  gType arrayObjType
                return $ NormalType sp inKernel "object"

--          where 
--                findType i vt = note (InstNotInScope i) (M.lookup i vt)
--                findTypeA i vt = note (InstNotInScope i) (M.lookup i vt)
-- WV: I removed the type check on Service objects
          where 
            findType i vt = note (InstNotInScope i) (Just (M.findWithDefault objectTypePos i vt))
            findTypeA i vt = note (InstNotInScope i) (Just  (M.findWithDefault arrayObjTypePos i vt))
          
    (ExpIdent i) -> note (NotInScope i) (M.lookup i vtable)

    (ExpLit l) -> return $ case l of
                Str s _ -> strTypePos notKernel s
                Ch s _ -> chTypePos notKernel s
                Number s (Left _) -> intTypePos notKernel s
                Number s (Right _) -> doubleTypePos notKernel s
                Bl s _ -> boolTypePos notKernel s

 where
    checkArgTypes :: [(Type SrcPos, Type SrcPos)] -> Either TypeScopeError ()
    checkArgTypes [] = return ()
    checkArgTypes (x:xs) = do 
        checkType' (fst x) (snd x)
        checkArgTypes xs


-- Get Type of a Binary Expression
getTypeBinOp :: VarTable -> FunTable -> BinOps SrcPos -> Expr SrcPos 
                         -> Expr SrcPos -> Either TypeScopeError (Type SrcPos)
getTypeBinOp vtable ftable bop e1 e2  = do
    leftType <- getTypeExpr vtable ftable e1
    rightType <- getTypeExpr vtable ftable e2
    -- If one of the expressions is in the kernel, cast the other
    -- one into the kernel
    let leftType' = if isInKernel rightType then castToKernel leftType else leftType
        rightType' = if isInKernel leftType then castToKernel rightType else rightType
    if isPointer leftType' || isPointer rightType' then
        getPointerTypeBin bop leftType' rightType'
    else getNormalTypeBin bop leftType' rightType'


-- Get Type of binary expression involving no pointers
getNormalTypeBin :: BinOps SrcPos -> Type SrcPos -> Type SrcPos -> Either TypeScopeError (Type SrcPos)
getNormalTypeBin bop leftType rightType
   | bop `elem` numNumNumOp =
       if leftType /= rightType then
           Left $ BinaryOpError bop "Both expressions expected to be the same type"
       else if stripAnnType leftType == intType' then return $ intTypePos kernel opPos
       else if stripAnnType leftType == doubleType' then return $ doubleTypePos kernel opPos
       else Left $ BinaryOpError bop "Expected integer or double type"

   | bop `elem` intIntIntOp =
       if (stripAnnType leftType, stripAnnType rightType) == (intType', intType')
           then return $ intTypePos kernel opPos
           else Left $ BinaryOpError bop "Expected integer values on both sides"

   | bop `elem` compareOp =
       if (stripAnnType leftType, stripAnnType rightType) `elem` 
            [(intType', intType'), (doubleType', doubleType')]
           then return $ boolTypePos kernel opPos
           else Left $ BinaryOpError bop "Expected numeric values of the same type"

   | bop `elem` boolOp =
       if (stripAnnType leftType, stripAnnType rightType) == (boolType', boolType')
           then return $ boolTypePos kernel opPos
           else Left $ BinaryOpError bop "Expected boolean values"

   | bop `elem` eqOp = 
        if leftType == rightType 
            then return $ boolTypePos kernel opPos
            else Left $ BinaryOpError bop "Expected equality of same types"

   | otherwise = Left $ BinaryOpError bop "compiler error"
  where
     numNumNumOp = [Add opPos, Sub opPos, Mul opPos, Div opPos]
     intIntIntOp = [Mod opPos, BAnd opPos, BOr opPos, BXor opPos, ShiftL opPos, ShiftR opPos]
     compareOp = [LessEq opPos, Less opPos, Greater opPos, GreaterEq opPos]
     eqOp = [Equals opPos, NEquals opPos]
     boolOp = [And opPos, Or opPos]
     intType' = intType kernel
     boolType' = boolType kernel
     doubleType' = doubleType kernel
     kernel = isInKernel leftType
     opPos = binOpPos bop


-- Get type of binary expression involving pointers
getPointerTypeBin :: BinOps SrcPos -> Type SrcPos -> Type SrcPos -> Either TypeScopeError (Type SrcPos)
getPointerTypeBin bop leftType rightType
    | bop == Add opPos =
        if isPointer leftType && stripAnnType rightType == intType' then
            return leftType
        else if isPointer rightType && stripAnnType leftType == intType' then
            return rightType
        else Left (BinaryOpError bop "Can only add Pointers to Integers")

    | bop == Sub opPos =
        if isPointer leftType && stripAnnType rightType == intType' then
        return leftType
        else Left (BinaryOpError bop
              "expected pointer type on lhs and integer type on rhs for pointer subtraction")

    | bop `elem` [Equals opPos, NEquals opPos] = case (leftType, rightType) of
        ((PointerType a, PointerType b)) ->
            if a == b then
                return $ boolTypePos kernel opPos
            else Left (BinaryOpError bop $ "Expected pointer types to be equal, left points to " ++ 
                    show a ++ ". Right points to " ++ show b ++ ".")

        _ -> Left (BinaryOpError bop 
                "Cannot perform an equality comparison of pointer and non pointer types")

    | otherwise =  Left (BinaryOpError bop $ "operation " ++ show bop ++ 
                        " not defined for pointer types")
  where
     intType' = intType kernel
     --boolType' = boolType kernel
     kernel = isInKernel leftType
     opPos = binOpPos bop


--Get type of unary expression
getTypeUnOp :: VarTable -> FunTable -> UnaryOps SrcPos -> Expr SrcPos  
                        -> Either TypeScopeError (Type SrcPos)
getTypeUnOp vtable ftable uOp expr = case uOp of

    BNot p -> getTypeExpr vtable ftable expr >>=
        \t -> case t of
            (NormalType _ kernel "int") -> return $ NormalType p kernel "int"
            e -> Left (UnaryOpError uOp $ "Expected integer expression, but found " ++ show e)
    
    Neg p -> getTypeExpr vtable ftable expr >>=
        \t -> case t of
            (NormalType _ kernel "int") -> return $ NormalType p kernel "int"
            (NormalType _ kernel "double") -> return $ NormalType p kernel "double"
            e -> Left (UnaryOpError uOp $ "Expected numerical expression, but found " ++ show e)

    Not p -> getTypeExpr vtable ftable expr >>=
        \t -> case t of
            (NormalType _ kernel "bool") -> return $ NormalType p kernel "bool"
            e -> Left (UnaryOpError uOp $ "Expected boolean expression, but found " ++ show e)



stripAnnType :: Type a -> Type ()
stripAnnType (PointerType t) = PointerType $ stripAnnType t
stripAnnType (NormalType _ a b) = NormalType () a b



boolType b = NormalType () b "bool"
boolTypePos b s = NormalType s b "bool"
intType b = NormalType () b "int"
intTypePos b s = NormalType s b "int"
--strType b = NormalType () b "string"
strTypePos b s = NormalType s b "string"
--chType b = NormalType () b "char"
chTypePos b s = NormalType s b "char"
doubleType b = NormalType () b "double"
doubleTypePos b s = NormalType s b "double"
objectType = NormalType () True "object"
arrayObjType = NormalType () True "objArray"

objectTypePos = NormalType (SrcPos 0 0) True "object"
arrayObjTypePos = NormalType (SrcPos 0 0) True "objArray"

-- Upcast a type to a Kernel type, if
-- the given type is already a Kernel type then
-- returns the given type
castToKernel :: Type a -> Type a
castToKernel (PointerType t) = PointerType $ castToKernel t
castToKernel (NormalType a _ n) = NormalType a True n

isInKernel :: Type a -> Bool
isInKernel (PointerType t) = isInKernel t
isInKernel (NormalType _ k _) = k

notKernel = False
inKernel = True

isPointer :: Type a -> Bool
isPointer (PointerType _) = True
isPointer _ = False

