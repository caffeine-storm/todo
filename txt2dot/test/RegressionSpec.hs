module RegressionSpec (spec) where

import Control.Monad (forM_)

import Test.Hspec
import Txt2Dot

validCases :: [String]
validCases = [regr1, regr2, regr3, regr4]

invalidCases :: [String]
invalidCases = []

regr1 :: String
regr1 = "- foo\nbar\n\tbaz\n\t\tmez"

regr2 :: String
regr2 = "- foo\nbar\n\tbaz"

regr3 :: String
regr3 = "- foo\n\tbaz"

regr4 :: String
--regr4 = "- foo\n\t- bar\n\t\tbaz"
regr4 = "- foo\n\t- bar\n\tbaz"

spec :: Spec
spec =
  describe "must not happen anymore" $ do
    it "should accept valid inputs (that used to be rejected)" $ do
      forM_ validCases $ \tcase ->
        (parseText tcase) `shouldNotBe` Nothing
    it "should reject invalid inputs (that is used to accept)" $ do
      forM_ invalidCases $ \tcase ->
        (parseText tcase) `shouldBe` Nothing
