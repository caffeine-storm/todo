module Parse.BlockSpec(spec) where

import Data.List (intercalate)

import Test.Hspec
import Txt2Dot (parseText)
import TodoGraph

blockExample :: String
blockExample = unlines [
  "root node; lvl1",
  "\tlvl2",
  "\t- start block; lvl2",
  "\t  continue block; lvl2",
  "\tlvl2 too"
  ]

oneLineBlock :: String
oneLineBlock = "- technically, a block"
oneLineBlockLabel :: String
oneLineBlockLabel = drop 2 oneLineBlock

blockAlone :: String
blockAlone = unlines [
  "- start block",
  "  continue block"
  ]
blockAloneLabel :: String
blockAloneLabel = intercalate "\n" [drop 2 line | line <- lines blockAlone]

spec :: Spec
spec = do 
  describe "parsing blocks" $ do
    it "recognizes a block as starting with '- ' and continued with '  '" $ do
      (parseText blockExample) `shouldBe` (Just $ TodoNode "root node; lvl1" [
        leafNode "lvl2",
        leafNode "start block; lvl2\ncontinue block; lvl2",
        leafNode "lvl2 too"
        ])
    it "can handle a single block as the whole input" $ do
      (parseText blockAlone) `shouldBe` (Just $ leafNode blockAloneLabel)
    it "can hanlde a sinlge-line block" $ do
      (parseText oneLineBlock) `shouldBe` (Just $ leafNode oneLineBlockLabel)
    it "can handle two blocks, one after the other" $ do
      (parseText (unlines [oneLineBlock, oneLineBlock])) `shouldBe`
        (Just $ TodoNode "" [leafNode oneLineBlockLabel, leafNode oneLineBlockLabel])
