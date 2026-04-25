module RegressionSpec (spec) where

import Control.Monad (forM_)

import Test.Hspec
import Txt2Dot

validCases :: [String]
validCases = [regr1, regr2, regr3]

invalidCases :: [String]
invalidCases = []

regr1 :: String
regr1 = "- foo\nbar\n\tbaz\n\t\tmez"

regr2 :: String
regr2 = "- foo\nbar\n\tbaz"

regr3 :: String
regr3 = "- foo\n\tbaz"

regr4 :: String
regr4 = "- foo\n\t- bar\n\t\tbaz"

naturals :: [Int]
naturals = [1..]

spec :: Spec
spec =
  it "must not happen anymore" $ do
    -- context "checking valid inputs" $ do
    forM_ (zip naturals validCases) $ \(n, tcase) ->
      --it ("should parse case " ++ (show n)) $ do
      (parseText tcase) `shouldNotBe` Nothing
    forM_ (zip naturals invalidCases) $ \(_, tcase) ->
      (parseText tcase) `shouldBe` Nothing
