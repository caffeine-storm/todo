module MainSpec (spec) where

import Test.Hspec
import Txt2Dot (parseText, TodoGraph(TodoNode))

-- ("1" [])
n1 :: TodoGraph
n1 = TodoNode "1" []

-- ("1_" ["1"])
n1_1 :: TodoGraph
n1_1 = TodoNode "1_" [n1]

-- ("2" [])
n2 :: TodoGraph
n2 = TodoNode "2" []

-- ("3" [])
n3 :: TodoGraph
n3 = TodoNode "3" []

spec :: Spec
spec = do
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
