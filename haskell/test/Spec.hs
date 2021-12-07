import Lib
import Test.Tasty
import Test.Tasty.HUnit


import Data
import Util
import qualified Data.StorableVector                        as SV

-- In this file, we will focus on testing the pure helper functions
--
-- We will not test the graphics, since that changes frequently and
--  doesn't have any formal requirements above "look sick"
-- 
-- This goes for the profile_compute and bar_compute functions too.



main :: IO ()
main =
  defaultMain $
    testGroup "Tests" $
        [ testCase "In-frame normalization" $ do
            in_frame_normalize [1.0]  @?= [1.0]
        , testCase "In-frame normalization" $ do
            in_frame_normalize [1.0, 0.0, 0.0, 0.0]  @?= [1.0, 0.0, 0.0, 0.0]
        , testCase "In-frame normalization" $ do
            in_frame_normalize [1.0, 1.0, 0.0, 0.0]  @?= [0.5, 0.5, 0.0, 0.0]
        , testCase "Rolling max" $ do
            rolling_max 5 [0, 1, 2, 3, 4, 5, 6, 7, 8] @?= [0, 1, 2, 3, 4, 5, 6, 7, 8]
        , testCase "Rolling max" $ do
            rolling_max 5 [0, 1, 2, 3, 4, 5, 5, 4, 8] @?= [0, 1, 2, 3, 4, 5, 5, 5, 8]
        , testCase "Rolling max" $ do
            rolling_max 5 [0, 1] @?= [0, 1]
        , testCase "Rolling max" $ do
            rolling_max 2 [10, 9, 8, 7, 6, 5] @?= [10, 10, 9, 8, 7, 6]
        , testCase "Rolling average" $ do
            rolling_average 2 [0, 0, 0, 0] @?= [0, 0, 0, 0]
        , testCase "Rolling average" $ do
            rolling_average 2 [0, 1, 1, 1] @?= [0, 0.5, 1, 1]
        , testCase "Rolling average" $ do
            rolling_average 3 [0, 1, 1, 1] @?= [0, 1/3, 2/3, 1]
        ]







{- main = defaultMain $ testGroup "Tests" $
  [ testCase "Addition works" $ do
      2 + 3 @?= 5  -- actual @?= expected
  , testCase "Multiplication works" $ do
      6 @=? 2 * 3  -- expected @=? actual
  -- NOTE: Uncomment this to see what a failing assertion looks like:
  -- , testCase "Bad assertion" $ do
  --     1 @?= 2
  -- NOTE: This is how to explicitly assert failures:
  -- , testCase "Explicit failure" $ do
  --     assertFailure "BOOM!"
  ]
-}
