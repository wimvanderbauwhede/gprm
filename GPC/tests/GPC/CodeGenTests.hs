{- Test Suite for Code Generating -}

module GPC.CodeGenTests (codeGenTests) where

import Test.HUnit
--import GPC.CodeGen
import GPC.Tests
import qualified Test.Framework.Providers.API as TFA
import Test.Framework.Providers.HUnit


-- | Return expected and actual program output
-- | These are expected to pass and to be equal
genAssignCheck :: [(String,String)]
genAssignCheck = [asInt]
 where
    -- |Check integer literal assignment
    asInt = ("placeholder", "placeholder")
   
generateTest :: String -> (String, String) -> TFA.Test
generateTest label (e, a) = testCase label $ assertEqual "" e a

-- | Generate test cases
generateTests :: String -> [(String, String)] -> [TFA.Test]
generateTests s ps = map (uncurry generateTest) $ zip labels ps 
    where labels = makeLabels s ps

  
-- | Test valid assignments 
assignGenTests :: [TFA.Test]
assignGenTests = generateTests "CodeGenAssignTest" genAssignCheck

-- | All Test cases to run
codeGenTests :: TFA.Test
codeGenTests = TFA.testGroup "CodeGenTests" $ concat [assignGenTests]
