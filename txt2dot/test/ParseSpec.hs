module ParseSpec (spec) where

import Test.Hspec
import Txt2Dot (parseText, TodoGraph(TodoNode), leafNode)

-- ("1" [])
n1 :: TodoGraph
n1 = leafNode "1"

-- ("1_" ["1"])
n1_1 :: TodoGraph
n1_1 = TodoNode "1_" [n1]

-- ("2" [])
n2 :: TodoGraph
n2 = leafNode "2"

-- ("3" [])
n3 :: TodoGraph
n3 = leafNode "3"

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
  describe "leafNode helper" $ do
    it "makes leaf nodes from a label string" $ do
      leafNode "1" `shouldBe` TodoNode "1" []
  describe "parsing" $ do
    it "can parse a single line" $ do
      (parseText "1") `shouldBe` (Just $ n1)
    it "can parse two root nodes" $ do
      (parseText "1\n2\n") `shouldBe` (Just $ TodoNode "" [n1, n2])
    it "can parse a subnode" $ do
      (parseText "1_\n\t1\n") `shouldBe` (Just $ n1_1)
    it "keeps root ordering in tact" $ do
      (parseText "1\n2\n3\n") `shouldBe` (Just $ TodoNode "" [n1, n2, n3])
    it "keeps subitem ordering in tact" $ do
      (parseText "1_\n\t1\n\t2\n") `shouldBe` (Just $ TodoNode "1_" [n1, n2])
    it "ignores empty lines" $ do
      (parseText "1_\n\n\t1\n2\n") `shouldBe` (Just $ TodoNode "" [n1_1, n2])
    it "treats whitespace-only lines as empty" $ do
      (parseText "1\n\t1_1\n\t\n\t1_2\n") `shouldBe` (Just $ TodoNode "1" [leafNode "1_1", leafNode "1_2"])
    it "recognizes a block as starting with '- ' and continued with '  '" $ do
      (parseText blockExample) `shouldBe` (Just $ TodoNode "root node; lvl1" [
        leafNode "lvl2",
        leafNode "start block; lvl2\ncontinue block; lvl2",
        leafNode "lvl2 too"
        ])
