{- Error Types -}
{-# LANGUAGE GADTs #-}

module GPC.Errors where

import GPC.AST
--import Control.Monad.Error

data TypeScopeError where   
     -- ^ Identifier not in scope
     NotInScope :: Ident SrcPos -> TypeScopeError 
     InstNotInScope :: Ident SrcPos -> TypeScopeError 
     -- ^ Duplicate Argument names in given Function
     DuplicateArgNames :: Ident SrcPos -> Ident SrcPos -> TypeScopeError   
     -- ^ Function expects no of args but was given a different number
     WrongNoArgs :: Ident SrcPos -> Int -> Int -> TypeScopeError    
     -- ^ Multiple Functions of the same name declared
     MultipleFunctionDecl :: Ident SrcPos -> TypeScopeError     
     -- ^ Type mismatch, expected, actual 
     TypeMismatch :: (Show a) => Type SrcPos -> a -> TypeScopeError     
     -- ^ Multiple instances of an identifier in current scope 
     MultipleInstances :: Ident SrcPos -> TypeScopeError   
     -- ^ Assigning to array   
     AssignArray :: Ident SrcPos -> TypeScopeError
     -- ^Attempting to index something which isn't an array  
     IndexingNotArray :: Ident SrcPos -> TypeScopeError     
     -- ^ Redefining in scope
     Redefine :: Ident SrcPos -> TypeScopeError
     -- ^ Binary operation error
     BinaryOpError :: BinOps SrcPos -> String -> TypeScopeError
     -- ^ Unary operation error
     UnaryOpError :: UnaryOps SrcPos -> String -> TypeScopeError 
     WrongNamespace :: [Ident SrcPos] -> [Ident SrcPos] -> TypeScopeError
     ExpectedConstructor :: [Ident SrcPos] -> [Ident SrcPos] -> TypeScopeError
--     OtherError :: String -> TypeScopeError


instance Show TypeScopeError where
    show (NotInScope i) = 
        errorIdent i ++ "Identifier \"" ++ show i ++ "\" not in scope" 

    show (InstNotInScope i) = 
        errorIdent i ++ "Instance \"" ++ show i ++ "\" not in scope" 

    show (DuplicateArgNames fI aI) = 
        errorIdent aI ++ "Duplicate instance of argument \"" ++ show aI ++
                         "\" in function definition for function \"" ++ show fI ++ "\"."
    
    show (WrongNoArgs fI expected actual) = 
        errorIdent fI ++ "Function \"" ++ show fI ++  "\" expects " ++ show expected ++
                      " arguments but was given " ++ show actual ++ "."

    show (MultipleFunctionDecl i) = 
        "Multiple definitions of function \"" ++ show i ++ "\"."

    show (TypeMismatch expected actual) = 
        errorType expected ++ "Expected type " ++ show expected ++ 
                              " but expression evaluated to type " ++ show actual ++ "."

    show (MultipleInstances i) = 
        errorIdent i ++ "Multiple instances of \"" ++ show i ++ 
                        "\" in current scope."

    show (AssignArray i) =
        errorIdent i ++ "Cannot assign to array \"" ++ show i ++ "\"."

    show (IndexingNotArray i) = 
        errorIdent i ++ "Attempting to access index of identifier \"" ++ show i ++ "\"" ++
                        "which is not an array."

    show (Redefine i) = 
        errorIdent i ++ "Cannot redefine identifier \"" ++ show i ++ "\"" ++
                        "in current scope." 

    show (BinaryOpError bOp message) = errorBinOp bOp ++ message

    show (UnaryOpError uOp message) = errorUnOp uOp ++ message

    show (WrongNamespace ns1 ns2) = "No Object(s) declared with namespace \"" ++
             show (init ns1) ++ "\" perhaps you meant " ++ show ns2 ++ "?"

    show (ExpectedConstructor ns1 ns2) = "Expected object constructor \"" ++
                show ns1 ++ "\" but found \"" ++ show ns2 ++ "\""

--    show (OtherError s) = s


--instance Error TypeScopeError where
--    noMsg = OtherError "Type/Scope error!"
--    strMsg = OtherError

type TypeErrorMonad = Either TypeScopeError

errorSrcPos :: SrcPos -> String
errorSrcPos (SrcPos line col) = "Error line:" ++ show line ++ " column:" ++ show col ++ "\n"

errorIdent :: Ident SrcPos -> String
errorIdent (Ident sp _) = errorSrcPos sp

errorType :: Type SrcPos -> String
errorType (PointerType t) = errorType t
errorType (NormalType sp _ _) = errorSrcPos sp

errorBinOp :: BinOps SrcPos -> String
errorBinOp = errorSrcPos . binOpPos 


binOpPos :: BinOps SrcPos -> SrcPos
binOpPos bop = case bop of
      Add a -> a
      Sub a -> a
      Mul a -> a
      Div a -> a
      And a -> a
      Or a  -> a
      Mod a -> a
      Less a  -> a
      LessEq a -> a
      Greater a -> a
      GreaterEq a -> a
      Equals a  -> a
      NEquals a -> a
      ShiftL a -> a
      ShiftR a -> a
      BAnd a -> a
      BXor a -> a
      BOr a -> a


errorUnOp :: UnaryOps SrcPos -> String
errorUnOp = errorSrcPos . unOpPos

unOpPos :: UnaryOps SrcPos -> SrcPos
unOpPos p = case p of
    Not a -> a
    Neg a -> a
    BNot a -> a 
