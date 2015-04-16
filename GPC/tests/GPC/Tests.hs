{- Common Test functions used in test modules -}
module GPC.Tests (makeLabels) where

-- | Make labels for test cases given a string for example given name
-- | assign, creates labels assign1 assign2 etc for tests
makeLabels :: String -> [a] -> [String]
makeLabels s xs = zipWith (++) prefixs $ map show $ take (length xs) ([1,2..] :: [Int])
  where prefixs = repeat s

