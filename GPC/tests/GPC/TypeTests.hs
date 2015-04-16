-- Unit Tests for the type checker

module GPC.TypeTests(typeTests) where

import Test.HUnit
import qualified Test.Framework.Providers.API as TFA
import Test.Framework.Providers.HUnit
import GPC.AST
import GPC.TypeScopeChecker
import GPC.Errors
import Control.Monad
import qualified Data.Map as M
import Data.Either

doubleConst :: Double -> Expr SrcPos
doubleConst a = ExpLit $ Number srcPos (Right a)

intConst :: Integer -> Expr SrcPos
intConst a = ExpLit $ Number srcPos (Left a)

--strConst :: String -> Expr SrcPos
--strConst s = ExpLit $ Str srcPos s 

isRight' = either (const False) (const True)

srcPos :: SrcPos
srcPos = SrcPos 0 0

intTypeNK = NormalType srcPos False "int"
intTypeK  = NormalType srcPos True  "int"
boolTypeNK = NormalType srcPos False "bool"
--boolTypeK = NormalType True "bool"

{-expressions = [intConst 20 -- ^ Check constant integer
              ,(ExpBinOp (Less srcPos) (intConst 10) (ExpIdent $ Ident srcPos "a")) -- ^ Check binary expression
              ,(ExpFunCall $ FunCall (Ident srcPos "fun1")  -- ^ Check Function call expression
                [intConst 10, ExpBinOp (Add srcPos) (intConst 20) (ExpIdent $ Ident srcPos "a")]) 
              ,strConst "hi" -- ^ Check string literal
              ,(ExpUnaryOp (BNot srcPos) (ExpIdent $ Ident srcPos "b")) -- ^ Check unary expression
              ] 
-}

{-expectedTypes = map (NormalType srcPos False) ["int"
                           ,"bool"
                           ,"int"
                           ,"string"
                           ,"int"
                           ]
-}

-- ^ Invalid expressions which are expected to give an error message
failExpressions = [(ExpBinOp (Less srcPos) (intConst 10) (doubleConst 20.0))
                  -- Check functions called with more args gives error
                  ,(ExpFunCall $ (FunCall (Ident srcPos "fun1") 
                    [(intConst 1), (intConst 1), (intConst 1)]))
                  ,(ExpFunCall $ (FunCall (Ident srcPos "fun1") [(intConst 1)]))
                  ]


-- Check that method calls cast the return type implicitly at compiler time
-- into a kernel type

programTempl stmts = Program 
    [TLObjs $ Objects (map (Ident srcPos) ["GPRM","Kernel","A"]) (VarIdent $ Ident srcPos "obj")    
    ,Func (intTypeNK) (Ident srcPos "test") [] $ BlockStmt stmts
    ]

methodCalls = map programTempl 
    [[AssignStmt $ Assign intTypeNK (Ident srcPos "i") 
        (ExpMethodCall (MethodCall (VarIdent $ Ident srcPos "obj") (Ident srcPos "m1") [intConst 5]))

     ,MethodStmt $ MethodCall (VarIdent $ Ident srcPos "obj") (Ident srcPos "m1") [intConst 32]
     ]
    ]

expectedTCMethodCalls = map programTempl 
    [[AssignStmt $ Assign intTypeK (Ident srcPos "i") 
        (ExpMethodCall (MethodCall (VarIdent $ Ident srcPos "obj") (Ident srcPos "m1") [intConst 5]))

     ,MethodStmt $ MethodCall (VarIdent $ Ident srcPos "obj") (Ident srcPos "m1") [intConst 32]
     ]
    ]

-- Check Pointers type checked correctly, and reduced
pointerProgramTempl stmts = Program [Func (intTypeNK) (Ident srcPos "test") 
                                [(PointerType intTypeNK, Ident srcPos "a")] $ BlockStmt stmts]
pointerAssigns = map pointerProgramTempl
    [[AssignStmt $ Assign (PointerType intTypeNK) (Ident srcPos "b")
        (ExpBinOp (Add srcPos) (ExpIdent $ Ident srcPos "a") 
            (ExpBinOp (Add srcPos) (intConst 4) (intConst 3)))
     ]
    ]

expectedTCPointerAssigns = map pointerProgramTempl
    [[AssignStmt $ Assign (PointerType intTypeNK) (Ident srcPos "b")
        (ExpBinOp (Add srcPos) (ExpIdent $ Ident srcPos "a") 
            (ExpBinOp (Add srcPos) (intConst 4) (intConst 3)))
     ]
    ]


-- Check that after assigning a method call result to a variable, that
-- that variable can't then be used in non-kernel ways 
invalidMethodUse = map programTempl 
                    [[(AssignStmt $ Assign intTypeNK (Ident srcPos "i") 
                        (ExpMethodCall (MethodCall (VarIdent $ Ident srcPos "obj") (Ident srcPos "m1") []))),
                      (AssignStmt $ Assign intTypeNK (Ident srcPos "j") (ExpIdent $ Ident srcPos "i"))]

                    -- check kernel Boolean can't be used in if
                    ,[(AssignStmt $ Assign boolTypeNK (Ident srcPos "i") 
                        (ExpMethodCall (MethodCall (VarIdent $ Ident srcPos "obj") (Ident srcPos "m1") []))),
                      (If (ExpIdent $ Ident srcPos "i") (BStmt $ BlockStmt []))]
                   ]


-- Check multiple variables can't be declared in the same scope
multipleDefInScope = 
    -- Check identifiers on top level scope not duplicated
    [Program [TLAssign (Assign intTypeNK (Ident srcPos "i") (intConst 5))
             ,TLAssign (Assign intTypeNK (Ident srcPos "i") (intConst 3))
             ]
     -- Check function arguments not duplicated    
     ,Program [Func (intTypeNK) (Ident srcPos "mulArgsTest") 
                [(intTypeNK, Ident srcPos "a"), (boolTypeNK, Ident srcPos "a")] $ BlockStmt []
              ]                
     -- Check function argument, not also present in main
     -- function block
     ,Program [Func (intTypeNK) (Ident srcPos "argsFunDups") [(boolTypeNK, Ident srcPos "b")] $ BlockStmt
                [AssignStmt $ Assign intTypeNK (Ident srcPos "b") (intConst 42)]
              ]
     ]                  

-- Check constructors which shouldn't compile
checkConstructors =
  map objTempl  
    -- Check assigning an object not declared doesn't work
    [[TLConstructObjs $ ConstructObjs 
        (map (Ident srcPos) ["GPRM", "Kernel", "A", "A"]) 
        (VarIdent $ Ident srcPos "e") []
     ]   
   -- Check constructor syntax is correct
   ,[TLConstructObjs $ ConstructObjs 
        (map (Ident srcPos) ["GPRM", "Kernel", "A", "C"]) 
        (VarIdent $ Ident srcPos "o") []
    ]
    -- Check constructing array element for something not in an array
   ,[TLConstructObjs $ ConstructObjs 
        (map (Ident srcPos) ["GPRM", "Kernel", "A", "A"]) 
        (VarArrayElem (Ident srcPos "o") (intConst 0)) []]

   -- Check assigning an array element of an object not declared doesn't
   -- work
   ,[TLConstructObjs $ ConstructObjs 
        (map (Ident srcPos) ["GPRM", "Kernel", "A", "D"]) 
        (VarArrayElem (Ident srcPos "e") (intConst 0)) []
    ]
   -- Check constructor syntax for array elements are correct
   ,[TLConstructObjs $ ConstructObjs 
        (map (Ident srcPos) ["GPRM", "Kernel", "A", "C"]) 
        (VarArrayElem (Ident srcPos "t") (intConst 0)) []
    ]
    -- Check constructing object for an array
   ,[TLConstructObjs $ ConstructObjs 
        (map (Ident srcPos) ["GPRM", "Kernel", "A", "D"]) 
        (VarIdent $ Ident srcPos "t") []
    ]
   -- Check out of bounds array access
   ,[TLConstructObjs $ ConstructObjs 
        (map (Ident srcPos) ["GPRM", "Kernel", "A", "D"]) 
        (VarArrayElem (Ident srcPos "t") (intConst 2)) []
    ]
   -- Check out of bounds array access (negative element)
   ,[TLConstructObjs $ ConstructObjs 
        (map (Ident srcPos) ["GPRM", "Kernel", "A", "D"]) 
        (VarArrayElem (Ident srcPos "t") (intConst (-1))) []
    ]
   ]
    where 
        objTempl :: [TopLevel SrcPos] -> Program SrcPos
        objTempl stmts = Program (
            [TLObjs $ Objects (map (Ident srcPos) ["GPRM", "Kernel", "A"]) 
                              (VarIdent $ Ident srcPos "o")
                                           
            ,TLObjs $ Objects (map (Ident srcPos) ["GPRM", "Kernel", "D"]) 
                              (VarArrayElem (Ident srcPos "t") (intConst 2))]
            ++ stmts)
                
-- Programs which should pass type checking
validPrograms = [Program [TLAssign (Assign intTypeNK (Ident srcPos "i") (intConst 5))
                         ,TLAssign (Assign intTypeNK (Ident srcPos "j") (ExpIdent (Ident srcPos "i")))
                         ]
                -- Check single object declarations and construction
                ,Program [TLObjs $ Objects (map (Ident srcPos) ["GPRM", "Kernel", "A"]) 
                                           (VarIdent $ Ident srcPos "o"),
                          TLConstructObjs $ ConstructObjs 
                            (map (Ident srcPos) ["GPRM", "Kernel", "A", "A"]) 
                            (VarIdent $ Ident srcPos "o")
                            [intConst 72] 
                         ]
                -- Check array object declarations and construction   
                ,Program [TLObjs $ Objects (map (Ident srcPos) ["GPRM", "Kernel", "A"]) 
                                           (VarArrayElem (Ident srcPos "o") (intConst 2)),
                          TLConstructObjs $ ConstructObjs 
                            (map (Ident srcPos) ["GPRM", "Kernel", "A", "A"]) 
                            (VarArrayElem (Ident srcPos "o") (intConst 1))
                            [intConst 72],
                            
                          TLConstructObjs $ ConstructObjs 
                            (map (Ident srcPos) ["GPRM", "Kernel", "A", "A"]) 
                            (VarArrayElem (Ident srcPos "o") (intConst 0))
                            [intConst 32] 
                           
                         ]
                ]


vars = M.fromList $ map (\(a,b) -> (Ident srcPos a, NormalType srcPos False b)) 
              [("a", "int")
              ,("b", "int")
              ,("c", "double")
              ,("test", "int")
              ]
ftable = M.fromList [(Ident srcPos "fun1", (NormalType srcPos False "int" , 
            map (NormalType srcPos False) ["int", "int"]))
                     ]

 
{-validTest :: (Type SrcPos) -> Either TypeScopeError () -> TFA.Test
validTest e a = testCase "type check passed test" (
 case a of
    Left err -> assertFailure $ show err
    Right p -> return ()
    )
-}

validProgramTest :: Program SrcPos -> TFA.Test
validProgramTest p = testCase "Checking full programs" (
    case result  of
        Left err -> assertFailure $ show err
        Right _ -> unless (isRight' result) $ 
            assertFailure "This should never happen")
 where result = runTypeChecker p

-- Given a Program and an Expected reduced Program,
-- type check and reduce the given program and assert the
-- type checking is correct and the reduced program
-- matches the expected reduced program
typeCheckAndReduceTest :: Program SrcPos -> Program SrcPos -> TFA.Test
typeCheckAndReduceTest inP _ = testCase "Checking type/scope and reduction" (
    case result of
        Left err -> assertFailure $ show err
        Right _ -> return ()
    )
  where result = runTypeChecker inP


-- Given a Program which should fail type checking
-- assert that it does actually fail when type checking.
invalidProgramTest :: Program SrcPos -> TFA.Test
invalidProgramTest p = testCase "error catching test for full Program" (
    unless (isLeft result) $ 
    assertFailure $ "Program should have contained type/scope error" ++ show result)
 where isLeft = null . rights . return
       result = runTypeChecker p


invalidTest :: Either TypeScopeError (Type SrcPos) -> TFA.Test
invalidTest a = testCase "Error catching test" (
    unless (isLeft a) $ 
    assertFailure "Expected test to fail")
 where isLeft = null . rights . return

typeTests :: TFA.Test
typeTests = TFA.testGroup "Type/Scope Tests" $ 
    (map (invalidTest . (getTypeExpr vars ftable) ) failExpressions) ++
    (map validProgramTest validPrograms) ++
    (map invalidProgramTest $ invalidMethodUse ++ multipleDefInScope ++ 
                              checkConstructors ) ++
    (map (\(expected, inProg) -> typeCheckAndReduceTest inProg expected) 
        (zip expectedTCMethodCalls methodCalls)) ++
    (map (\(expected, inProg) -> typeCheckAndReduceTest inProg expected) 
        (zip expectedTCPointerAssigns pointerAssigns))
