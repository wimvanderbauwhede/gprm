{- Unit Tests for parser -}
module GPC.ParserTests(parserTests) where

import Data.Either
import qualified Test.Framework.Providers.API as TFA
import Test.Framework.Providers.HUnit
import Test.HUnit
import Control.Monad
import GPC.Tests
import GPC.Parser
import GPC.AST

srcPos = SrcPos 0 0
type ProgramPos = Program SrcPos


identPos = Ident srcPos

constructObjsCheck :: [(ProgramPos, Either String ProgramPos)]
constructObjsCheck = [singleObj, indexedObj]
  where 
    singleObj = 
        (Program 
            [TLConstructObjs $ 
                ConstructObjs (map identPos ["Test", "A", "A"]) (VarIdent $ identPos "a") []
            ]
        ,parseSource "a = Test::A::A();")
    
    indexedObj = 
        (Program 
            [TLConstructObjs $
                ConstructObjs (map identPos ["Test", "Blah", "B", "B"]) 
                    (VarArrayElem (identPos "b") (ExpLit $ Number srcPos (Left 10))) []
            ]
        ,parseSource "b[10] = Test::Blah::B::B();")

-- | Check declaring objects and arrays of objects works
objectsCheck :: [(ProgramPos , Either String ProgramPos)]
objectsCheck = [singleObj, largeNsSingleObj, arrObjs, arrObjsSp,
                spaceNameSpace1, spaceNameSpace2]
 where
   singleObj = (Program [TLObjs (Objects [identPos "Test", identPos "Obj"] (VarIdent $ identPos "obj"))],
               parseSource "Test::Obj obj;")

   largeNsSingleObj = (Program [TLObjs (Objects (map identPos ["Test1", "Test2", "Test3", "Test4", "Obj"]) 
                            (VarIdent $ identPos "obj"))],
                      parseSource "Test1::Test2::Test3::Test4::Obj obj;")

   arrObjs = (Program [TLObjs (Objects [identPos "Test", identPos "Obj"] 
                (VarArrayElem (identPos "obj") (ExpLit (Number srcPos (Left 10)))))],
               parseSource "Test::Obj obj[10];")
   
   -- Check space between array and name is skipped over
   arrObjsSp = (Program [TLObjs (Objects [identPos "Test", identPos "Obj"] 
                (VarArrayElem (identPos "obj") (ExpLit (Number srcPos (Left 10)))))],
               parseSource "Test::Obj obj [10];")

   -- Check spaces in namespace are skipped over
   spaceNameSpace1 = (Program [TLObjs (Objects (map identPos ["Test", "A", "B"]) (VarIdent $ identPos "d"))]
                     ,parseSource "Test   ::A::B d;")

   spaceNameSpace2 = (Program [TLObjs (Objects (map identPos ["Test", "A", "B", "C"]) 
                        (VarIdent $ identPos "g"))],
                      parseSource "Test::A::B  ::C g;")


invalidObjsCheck :: [Either String ProgramPos]
invalidObjsCheck = map parseSource [noNameSpace1, noNameSpace2, noVar] 
  where    
    noNameSpace1 = "Test a;"
    noNameSpace2 = "Stuff b[4];"
    noVar = "Test::A::B ;"


-- | Return expected and actual programs
-- | These are expected to pass and to be equal
assignCheck :: [(ProgramPos, Either String ProgramPos)]
assignCheck = [asInt, asChr, asBool, asDouble, asStr, asId]
 where
    -- |Check integer literal assignment
    asInt = (Program [TLAssign (Assign (NormalType srcPos False "int") (identPos "x") (ExpLit (Number srcPos (Left 20))))], 
            parseSource "int x = 20;")
    -- |Check char literal assignment
    asChr = (Program [TLAssign (Assign (NormalType srcPos False "char") (identPos "y") (ExpLit (Ch srcPos 'c')))], 
            parseSource "char y = 'c';")
    -- |Check bool literal assignment 
    asBool = (Program [TLAssign (Assign (NormalType srcPos False "bool") (identPos "b") (ExpLit (Bl srcPos False)))], 
            parseSource "bool b = false;")
    -- |Check float literal assignment
    asDouble = (Program [TLAssign (Assign (NormalType srcPos False "double") (identPos "d") (ExpLit (Number srcPos (Right 20.4))))], 
            parseSource "double d = 20.4;")
    -- |Check string literal assignment
    asStr = (Program [TLAssign (Assign (NormalType srcPos False "string") (identPos "s") (ExpLit (Str srcPos "hi")))], 
            parseSource "string s = \"hi\";")
    -- |Check identifier assignment
    asId = (Program [TLAssign (Assign (NormalType srcPos False "int") (identPos "i") (ExpIdent (identPos "x")))], 
            parseSource "int i =  x;")

-- | List of ProgramPoss which should fail assignment by the parser
assignFailCheck :: [Either String ProgramPos]
assignFailCheck = [noSemi, noAssign]
 where
    -- | No semicolon at end of statement
   noSemi = parseSource "int x ="
    -- | No equals, variables are single assignment
   noAssign = parseSource "float y;"
       
-- | Check binary operators are individually parsed
binOpCheck :: [(ProgramPos, Either String ProgramPos)]
binOpCheck = [ asMul, asEq, asShift, asPrece, asPrece2
             , asPrece3, asPrece4, asPrece5, asPrece6
             , asPrece7, asPrece8, parensCheck]
 where
    -- Check multiplication assignment of 2 identities
    asMul = (Program [TLAssign (Assign (NormalType srcPos False "int") (identPos "i") 
            (ExpBinOp (Mul srcPos) (ExpIdent (identPos "x")) (ExpIdent (identPos "y"))))] 
            ,parseSource "/* Check comment ignored */ int i = x * y;")
    -- Check equality assignment of a literal and identity
    asEq = (Program [TLAssign (Assign (NormalType srcPos False "bool") (identPos "b")
           (ExpBinOp (Equals srcPos) (ExpLit (Bl srcPos True)) (ExpIdent (identPos "b"))))]
           ,parseSource "bool b = true == b;")
    -- Check shift assignment of 3 values 
    asShift = (Program [TLAssign (Assign (NormalType srcPos False "int") (identPos "i")
            (ExpBinOp (ShiftL srcPos) ( ExpBinOp (ShiftL srcPos) (ExpLit (Number srcPos (Left 4))) (ExpLit (Number srcPos (Left 3))))
                   (ExpLit (Number srcPos (Left 2)))))]
            ,parseSource "int i = 4 << 3 << 2;")
    -- Check operator precedence works
    asPrece = (Program [TLAssign (Assign (NormalType srcPos False "int") (identPos "j")
              (ExpBinOp (Add srcPos) (ExpIdent (identPos "a")) (ExpBinOp (Mul srcPos) (
                ExpIdent (identPos "b")) (ExpIdent (identPos "c")))))]
              ,parseSource "int j = a + b * c;") 

    asPrece2 = (Program [TLAssign (Assign (NormalType srcPos False "int") (identPos "k")
               (ExpBinOp (Add srcPos) (ExpBinOp (Mul srcPos) 
               (ExpIdent (identPos "a")) (ExpIdent (identPos "b"))) (ExpIdent (identPos "c"))))]
               ,parseSource "int k = a * b + c;")

    asPrece3 = (Program [TLAssign (Assign (NormalType srcPos False "bool") (identPos "k")
               (ExpBinOp (Equals srcPos) (ExpBinOp (Div srcPos) 
               (ExpIdent (identPos "a")) (ExpIdent (identPos "b"))) (ExpIdent (identPos "c"))))]
               ,parseSource "bool k = a / b == c;")

    asPrece4 = (Program [TLAssign (Assign (NormalType srcPos False "int") (identPos "k")
               (ExpBinOp (ShiftL srcPos) (ExpBinOp (Mod srcPos)
               (ExpIdent (identPos "a")) (ExpIdent (identPos "b"))) (ExpIdent (identPos "c"))))]
               ,parseSource "int k = a % b << c;")

    asPrece5 = (Program [TLAssign (Assign (NormalType srcPos False "bool") (identPos "k")
               (ExpBinOp (Less srcPos) (ExpBinOp (ShiftR srcPos)
               (ExpIdent (identPos "a")) (ExpIdent (identPos "b"))) (ExpIdent (identPos "c"))))]
               ,parseSource "bool k = a >> b < c;")

    asPrece6 = (Program [TLAssign (Assign (NormalType srcPos False "int") (identPos "k")
               (ExpBinOp (BOr srcPos) 
                 (ExpBinOp (BXor srcPos)
                    (ExpBinOp (BAnd srcPos) (ExpIdent (identPos "a")) (ExpIdent (identPos "b"))) 
                    (ExpIdent (identPos "c"))
                 ) 
                 (ExpIdent (identPos "d"))
               ))]
               ,parseSource "int k = a & b ^ c | d;")
   
    asPrece7 = (Program [TLAssign (Assign (NormalType srcPos False "bool") (identPos "k")
               (ExpBinOp (Or srcPos)
                 (ExpBinOp (And srcPos) 
                     (ExpBinOp (LessEq srcPos)  (ExpIdent (identPos "a")) (ExpIdent (identPos "b")))
                     (ExpBinOp (Greater srcPos) (ExpIdent (identPos "c")) (ExpIdent (identPos "d")))
                 )
                 (ExpBinOp (GreaterEq srcPos) (ExpIdent (identPos "e")) (ExpIdent (identPos "f")))
               )
               )]
               ,parseSource "bool k = a <= b && c > d || e >= f;")
    
    asPrece8 = (Program [TLAssign (Assign (NormalType srcPos False "bool") (identPos "k")
               (ExpBinOp (NEquals srcPos) (ExpBinOp (ShiftR srcPos)
               (ExpIdent (identPos "a")) (ExpIdent (identPos "b"))) (ExpIdent (identPos "c"))))]
               ,parseSource "bool k = a >> b != c;")

    -- Check precedence with parens
    parensCheck = (Program [TLAssign (Assign (NormalType srcPos False "int")  (identPos "l")
                  (ExpBinOp (Mul srcPos) (ExpIdent (identPos "a")) (ExpBinOp (Add srcPos)
                    (ExpIdent (identPos "b")) (ExpIdent (identPos "c")))))]
                  ,parseSource "int l = a * (  b +  c);") 
    
       
-- | Check unary operators are individually parsed
unOpCheck :: [(ProgramPos, Either String ProgramPos)]
unOpCheck = [asNot, asPrece, asPrece2, asParens]
 where
    -- Check assignment of not identity
    asNot = (Program [TLAssign (Assign (NormalType srcPos False "bool") (identPos "i") 
            (ExpUnaryOp (Not srcPos) (ExpIdent (identPos "x"))))] 
            ,parseSource "bool i = !x;")
    -- Check precedence with binary operators   
    asPrece = (Program [TLAssign (Assign (NormalType srcPos False "int") (identPos "j")
             (ExpBinOp (Add srcPos) (ExpIdent (identPos "a")) (ExpUnaryOp (BNot srcPos) (ExpIdent (identPos "b")))))]
             ,parseSource "int j = a + ~b;")    
    -- Check precedence with binary operators   
    asPrece2 = (Program [TLAssign (Assign (NormalType srcPos False "int") (identPos "j")
             (ExpBinOp (Add srcPos) (ExpUnaryOp (BNot srcPos) (ExpIdent (identPos "a"))) (ExpIdent (identPos "b"))))]
             ,parseSource "int j = ~ a + b;")
    -- Check precedence with parenthesis
    asParens = (Program [TLAssign (Assign (NormalType srcPos False "int")  (identPos "k")
               (ExpUnaryOp (Neg srcPos) (ExpBinOp (Sub srcPos) (ExpIdent (identPos "a")) (ExpIdent (identPos "b")))))]
               ,parseSource "int k = -(a - b);")


-- | Check function calls made within functions are correctly parsed
funCallCheck :: [(ProgramPos, Either String ProgramPos)]
funCallCheck = [noArgs, singleArgs, multiArgs, multiComplexArgs, standAlone]
 where
    -- Check function with no arguments
    noArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType srcPos False "int") (identPos "i")
             (ExpFunCall $ FunCall (identPos "test") []))]]
             ,parseSource $ funStr ++ "int i = test();" ++ "}")
    -- Check function with one argument
    singleArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType srcPos False "test") (identPos "j")
                 (ExpFunCall $ FunCall (identPos "func") [ExpIdent (identPos "a")]))]]
                 ,parseSource $ funStr ++ "test j = func(a);" ++ "}")
    -- Check function with multiple arguments
    multiArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType srcPos False "blarg") (identPos "m")
                (ExpFunCall $ FunCall (identPos "destroyAllHumans") [ExpIdent (identPos "a"), ExpIdent (identPos "b")]))]]
                ,parseSource$ funStr ++ "blarg m = destroyAllHumans(a, b);" ++ "}")
    -- Check function with multiple arguments with expressions
    multiComplexArgs = (Program [ fun $ [AssignStmt $ (Assign (NormalType srcPos False "int") (identPos "a")
                      (ExpFunCall $ FunCall (identPos "call") [ExpBinOp (Mul srcPos) (ExpIdent (identPos "a")) (ExpIdent (identPos "b")),
                      ExpUnaryOp (Neg srcPos) (ExpIdent (identPos "c"))]))]]
                      ,parseSource $ funStr ++ "int a = call(a * b, -c);" ++ "}")

    -- Check function call statement
    standAlone = (Program [fun [FunCallStmt $ FunCall (identPos "call") []]]
                  ,parseSource $ funStr ++ "call();" ++ "}")

    fun xs = Func (NormalType srcPos False "tes") (identPos "test") [] (BlockStmt xs)
    funStr = "tes test() {"


-- | Check method calls made within functions are correctly parsed
methodCallCheck :: [(ProgramPos, Either String ProgramPos)]
methodCallCheck = [noArgs, singleArgs, multiArgs, multiComplexArgs, standAlone, standAloneIndexed]
 where
    -- Check method with no arguments
    noArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType srcPos False "int") (identPos "i")
             (ExpMethodCall $ MethodCall (VarIdent $ identPos "a") (identPos "test") []))]]
             ,parseSource $ funStr ++ "int i = a.test();" ++ "}")
    -- Check method with one argument
    singleArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType srcPos False "test") (identPos "j")
                 (ExpMethodCall $ MethodCall (VarIdent $ identPos "a") (identPos "func") 
                    [ExpIdent (identPos "a")]))]]
                 ,parseSource $ funStr ++ "test j = a.func(a);" ++ "}")
    -- Check method with multiple arguments
    multiArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType srcPos False "blarg") (identPos "m")
                (ExpMethodCall $ MethodCall (VarIdent $ identPos "a") (identPos "destroyAllHumans") 
                    [ExpIdent (identPos "a"), ExpIdent (identPos "b")]))]]
                ,parseSource$ funStr ++ "blarg m = a.destroyAllHumans(a, b);" ++ "}")
    -- Check method with multiple arguments with expressions
    multiComplexArgs = (Program [ fun $ [AssignStmt $ (Assign (NormalType srcPos False "int") (identPos "a")
                      (ExpMethodCall $ MethodCall (VarIdent $ identPos "a") (identPos "call") 
                        [ExpBinOp (Mul srcPos) (ExpIdent (identPos "a")) (ExpIdent (identPos "b"))
                        ,ExpUnaryOp (Neg srcPos) (ExpIdent (identPos "c"))]))
                        ]]
                      ,parseSource $ funStr ++ "int a = a.call(a * b, -c);" ++ "}")

    -- Check method call statement
    standAlone = (Program [fun [MethodStmt $ MethodCall (VarIdent $ identPos "a") (identPos "call") []]]
                  ,parseSource $ funStr ++ "a.call();" ++ "}")
    --
    standAloneIndexed = 
        (Program [fun [MethodStmt $ MethodCall (VarArrayElem (identPos "a") (ExpLit (Number srcPos $ Left 2))) 
            (identPos "call") []]]
        ,parseSource $ funStr ++ "a[2].call();" ++ "}")

    fun xs = Func (NormalType srcPos False "tes") (identPos "test") [] (BlockStmt xs)
    funStr = "tes test() {"

-- | Check sequential/parallel blocks are correctly parsed
seqParBlockCheck :: [(ProgramPos, Either String ProgramPos)]
seqParBlockCheck = [seqB, parB, seqMultiB]
 where
    seqB = (Program [fun [(Seq $ BlockStmt [AssignStmt $ Assign 
            (NormalType srcPos False "int") (identPos "i") (ExpIdent (identPos "x"))])]],
           parseSource  $ funStr ++ "seq {int i = x;}" ++ "}")
    
    parB = (Program [fun [(BStmt $ BlockStmt [AssignStmt $ Assign 
            (NormalType srcPos False "int") (identPos "i") (ExpIdent (identPos "x"))])]],
           parseSource $ funStr  ++ "par {int i = x;}" ++ "}")

    seqMultiB = (Program [fun [ (Seq $ BlockStmt [AssignStmt $ Assign 
            (NormalType srcPos False "int") (identPos "i") (ExpIdent (identPos "x")),
           AssignStmt $ Assign (NormalType srcPos False "int") (identPos "j") (ExpIdent (identPos "y"))])]],
           parseSource $ funStr ++ "seq {int i = x; int j = y;}" ++ "}")
    fun xs = Func (NormalType srcPos False "tes") (identPos "test") [] (BlockStmt xs)
    funStr = "tes test() {"

-- | Check If-Else statements are correctly parsed
ifElseCheck :: [(ProgramPos, Either String ProgramPos)]
ifElseCheck = [ifCheck, elseCheck]
 where
    -- Check If by itself
    ifCheck = (Program [fun [ 
              (If (ExpUnaryOp (Not srcPos) (ExpIdent (identPos "x"))) 
                (BStmt $ BlockStmt [Return $ ExpIdent (identPos "y")])
              )]],
              parseSource $ funStr ++ "if (!x) {return y;}" ++ "}") 
    -- Check If with Else
    elseCheck = (Program [fun [
                (IfElse (ExpUnaryOp (Not srcPos) (ExpIdent (identPos "z")))
                    (Return $ ExpIdent (identPos "y"))
                    (Return $ ExpIdent (identPos "a"))
                )]],
                parseSource $ funStr ++ "if (!z) return y; else return a;" ++ "}")
    fun xs = Func (NormalType srcPos False "tes") (identPos "test") [] (BlockStmt xs)
    funStr = "tes test() {"

-- | Check For loop statements are correctly parsed
forLoopCheck :: [(ProgramPos, Either String ProgramPos)]
forLoopCheck = [forCheck]
  where
    forCheck = (Program [fun [ 
              (ForLoop (identPos "a") (ExpUnaryOp (Neg srcPos) (ExpLit $ Number srcPos $ Left 17)) 
                (ExpBinOp (LessEq srcPos) (ExpIdent $ identPos "a") (ExpLit $ Number srcPos $ Left 16)) (ExpLit $ Number srcPos $ Left 1) (BlockStmt []))]] 
              , parseSource $ funStr ++ "for (int a = -17; a <= 16; a+=1) {}" ++ "}") 

    fun xs = Func (NormalType srcPos False "tes") (identPos "test") [] (BlockStmt xs)
    funStr = "tes test() {"
    

-- | Check Pointer
pointersCheck :: [(ProgramPos, Either String ProgramPos)]
pointersCheck = [singlePtr, mulPtr]
  where
     singlePtr = (Program [Func intType (identPos "test") [(PointerType intType, a)] 
                            $ BlockStmt [AssignStmt 
                                $ Assign (PointerType intType) b (ExpIdent a)]]
                 ,parseSource "int test(int *a) {int *b = a;}") 


     mulPtr = (Program [Func intType (identPos "test") [(triplePtr intType, a)] 
                            $ BlockStmt [AssignStmt 
                                $ Assign (triplePtr intType) b (ExpIdent a)]]
                 ,parseSource "int test(int ***a) {int ***b = a;}") 

     intType = NormalType srcPos False "int"
     triplePtr = PointerType . PointerType . PointerType
     a = identPos "a"
     b = identPos "b"

validTest :: String -> (ProgramPos, Either String ProgramPos) -> TFA.Test
validTest label (e, a) = testCase label (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" e p)

validTests :: String -> [(ProgramPos, Either String ProgramPos)] -> [TFA.Test]
validTests s ps = map (uncurry validTest) $ zip labels ps
    where labels = makeLabels s ps

validObjectsTests :: [TFA.Test]
validObjectsTests = validTests "validObjectDecl" objectsCheck

validObjectConsTests :: [TFA.Test]
validObjectConsTests = validTests "validObjectCons" constructObjsCheck


-- | Test valid assignments 
validAssignTests :: [TFA.Test]
validAssignTests = validTests "validAssignTest" assignCheck

-- | Test valid assignments with binary operators
validOpTests :: [TFA.Test]
validOpTests = validTests "validOpTest" binOpCheck

-- | Test valid assignments with unary operators
validUnOpTests :: [TFA.Test]
validUnOpTests = validTests "validUnOpTest" unOpCheck

-- | Test valid function call expressions/statements
validFunCallTests :: [TFA.Test]
validFunCallTests = validTests "validFunCallTest" funCallCheck

-- | Test valid method call expressions/statements
validMethodCallTests :: [TFA.Test]
validMethodCallTests = validTests "validMethodCallTest" methodCallCheck

-- | Test valid sequential/parallel blocks
validSeqParTests :: [TFA.Test]
validSeqParTests = validTests "seqParTest" seqParBlockCheck

-- | Test valid if/else statements
validIfElseTests :: [TFA.Test]
validIfElseTests = validTests "ifElseTest" ifElseCheck

-- | Test valid for loops
validForLoopTests :: [TFA.Test]
validForLoopTests = validTests "forLoopTest" forLoopCheck

-- | Test Pointer Parsing
validPointerTests :: [TFA.Test]
validPointerTests = validTests "pointerTest" pointersCheck

-- | Test invalid statement
invalidTest :: String -> Either String ProgramPos -> TFA.Test
invalidTest label a = testCase label (
    unless (isLeft a) $ 
    assertFailure $ "ProgramPos should have caused a parse error" ++ show a)
 where isLeft = null . rights . return

invalidAssignTests :: [TFA.Test]
invalidAssignTests = map (uncurry invalidTest) $ zip labels assignFailCheck
 where labels = makeLabels "invalidAssignTest" assignFailCheck

invalidObjectsTests :: [TFA.Test]
invalidObjectsTests = map (uncurry invalidTest) $ zip labels invalidObjsCheck
 where labels = makeLabels "invalidObjDeclTest" invalidObjsCheck


parserTests = TFA.testGroup "Parser Tests" $ concat
    [validAssignTests, invalidAssignTests
    ,validOpTests, validUnOpTests
    ,validFunCallTests, validSeqParTests
    ,validIfElseTests, validForLoopTests, validObjectsTests
    ,validObjectConsTests, validMethodCallTests
    ,invalidObjectsTests, validPointerTests
    ]
