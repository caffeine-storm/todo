module Parse.BasicSpec (spec) where

import Test.Hspec
import Txt2Dot (parseText)
import TodoGraph

-- ("1" [])
leaf1 :: TodoGraph
leaf1 = leafNode "1"

-- ("1_" ["1"])
n1_leaf1 :: TodoGraph
n1_leaf1 = TodoNode "1_" [leaf1]

-- ("2" [])
leaf2 :: TodoGraph
leaf2 = leafNode "2"

-- ("3" [])
leaf3 :: TodoGraph
leaf3 = leafNode "3"

regression1Text :: String
regression1Text = unlines [
    "n1",
    "n2",
    "\tn2_1",
    "\t\tn2_1_1",
    "\tn2_2",
    "\t\tn2_2_1"
  ]
regression1Answer :: Maybe TodoGraph
regression1Answer = Just $ TodoNode "" [n1, n2]
  where
    n1 = leafNode "n1"
    n2 = TodoNode "n2" [n2_1, n2_2]
    n2_1 = TodoNode "n2_1" [n2_1_1]
    n2_1_1 = TodoNode "n2_1_1" []
    n2_2 = TodoNode "n2_2" [n2_2_1]
    n2_2_1 = TodoNode "n2_2_1" []

spec :: Spec
spec = do
  describe "leafNode helper" $ do
    it "makes leaf nodes from a label string" $ do
      leafNode "1" `shouldBe` TodoNode "1" []
  describe "parsing" $ do
    it "can parse a single line" $ do
      (parseText "1") `shouldBe` (Just $ leaf1)
    it "can parse two root nodes" $ do
      (parseText "1\n2\n") `shouldBe` (Just $ TodoNode "" [leaf1, leaf2])
    it "can parse a subnode" $ do
      (parseText "1_\n\t1\n") `shouldBe` (Just $ n1_leaf1)
    it "can parse sub-subnodes" $ do
      (parseText regression1Text) `shouldBe` regression1Answer
    it "keeps root ordering in tact" $ do
      (parseText "1\n2\n3\n") `shouldBe` (Just $ TodoNode "" [leaf1, leaf2, leaf3])
    it "keeps subitem ordering in tact" $ do
      (parseText "1_\n\t1\n\t2\n") `shouldBe` (Just $ TodoNode "1_" [leaf1, leaf2])
    it "ignores empty lines" $ do
      (parseText "1_\n\n\t1\n2\n") `shouldBe` (Just $ TodoNode "" [n1_leaf1, leaf2])
    it "treats whitespace-only lines as empty" $ do
      (parseText "1\n\t1_1\n\t\n\t1_2\n") `shouldBe` (Just $ TodoNode "1" [leafNode "1_1", leafNode "1_2"])
