module Parse.BlockSpec(spec) where

import Test.Hspec
import Txt2Dot (parseText, TodoGraph(TodoNode), leafNode)

blockExample :: String
blockExample = unlines [
  "root node; lvl1",
  "\tlvl2",
  "\t- start block; lvl2",
  "\t  continue block; lvl2",
  "\tlvl2 too"
  ]

spec :: Spec
spec = do 
  describe "parsing blocks" $ do
    it "recognizes a block as starting with '- ' and continued with '  '" $ do
      (parseText blockExample) `shouldBe` (Just $ TodoNode "root node; lvl1" [
        leafNode "lvl2",
        leafNode "start block; lvl2\ncontinue block; lvl2",
        leafNode "lvl2 too"
        ])
