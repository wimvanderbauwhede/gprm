-- Attempt to run the entire compiler on
-- source files, check files in the pass file
-- successfully compile, and that fails in the
-- fail folder, fail to compile at some point

module GPC.CompileTests (compileTests) where

import qualified Test.Framework.Providers.API as TFA
import Test.Framework.Providers.HUnit
import Test.HUnit
import System.Directory

import Data.List

import GPC.Parser
import GPC.TypeScopeChecker
import GPC.Interpreter
import GPC.SimplifyAST
import GPC.CodeGen

passDir =  "tests/examples/pass/"
failDir =  "tests/examples/fail/"
threads = 1

sourceFiles :: String -> IO [String]
sourceFiles dir = do 
    files <- getDirectoryContents dir
    return $ filter (isSuffixOf ".gpc") files


passTests :: IO TFA.Test
passTests = do 
    filePaths <- sourceFiles passDir
    let tests = map passTest filePaths
    return $ TFA.testGroup "Pass Tests" tests

passTest :: String -> TFA.Test
passTest file = testCase file ( do
    source <- readFile filePath
    case parseSource source of
        Left err -> assertFailure err
        Right v ->  do 
            case runTypeChecker v of
                Left err -> assertFailure $ show err
                Right () -> 
                    case genGPIR fileName (simplifyAST v) threads of
                        Left err -> assertFailure err
                        Right s -> do 
                            writeFile "test.out" (genCode s)
                            (return ())
    )
 where
    filePath = passDir ++ file 
    fileName = reverse $ drop 4 $ reverse file

failTests :: IO TFA.Test
failTests = do 
    filePaths <- sourceFiles failDir
    let tests = map failTest filePaths
    return $ TFA.testGroup "Fail Tests" tests

failTest :: String -> TFA.Test
failTest file = testCase file ( do
    source <- readFile filePath
    case parseSource source of
        Left _ -> return () 
        Right ast ->  do 
            case runTypeChecker ast of
                Left err -> do
                    writeFile "test.out" (show err)
                    return ()
                Right _ -> case genGPIR fileName (simplifyAST ast) threads of
                    Left err' -> do
                        writeFile "test.out" err'
                        return ()
                    Right e -> assertFailure $ "Program shouldn't have compiled " ++ show e
    )
 where
    filePath = failDir ++ file 
    fileName = reverse $ drop 4 $ reverse file

compileTests =  
    TFA.testGroup "Compiler Tests" $ map TFA.buildTest [passTests, failTests]
