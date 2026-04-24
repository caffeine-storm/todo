module ParseLineSpec (spec) where

import Test.Hspec
import ParseLine (parseLine, TodoLine(..))

spec :: Spec
spec = do
  describe "parsing a line of text" $ do
    it "should skip empty lines" $ do
      (parseLine "") `shouldBe` Skip
    it "should skip lines of just tabs or spaces" $ do
      (parseLine "\t") `shouldBe` Skip
      (parseLine "  ") `shouldBe` Skip
    it "should handle a root-node line" $ do
      (parseLine "some node") `shouldBe` (Line 0 "some node")
    it "should handle a sub-node line" $ do
      (parseLine "\t\tsub-sub node") `shouldBe` (Line 2 "sub-sub node")
    it "should recognize a section-start" $ do
      (parseLine "\t- section starter") `shouldBe` (SectionStart 1 "section starter")
    it "should recognize a section-continuation" $ do
      (parseLine "\t\t  section continued") `shouldBe` (SectionContinue 2 "section continued")
