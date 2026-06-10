module TodoGraphSpec(spec) where

import Test.Hspec
import TodoGraph
import Data.List.Ordered (isSorted)

someLeaf :: TodoGraph
someLeaf = leafNode "some label"

makeParent :: [String] -> TodoGraph
makeParent kidLabels = TodoNode "makeParent label" $ map leafNode kidLabels

someParent :: TodoGraph
someParent = makeParent ["kid1", "kid2", "kid3"]

someGrandParent :: TodoGraph
someGrandParent = TodoNode "grand parent label" [
  makeParent ["1.a", "1.b", "1.c"]
  , makeParent ["2.a", "2.b", "2.c"]
  , makeParent ["3.a", "3.b", "3.c"]]

isReverseSorted :: [TodoGraph] -> Bool
isReverseSorted = isSorted . map getLabel . reverse

spec :: Spec
spec = do
  describe "TodoGraph operations" $ do
    describe "mirroring node orderings" $ do
      it "leaves leaf nodes unchanged" $ do
        mirrorNodes someLeaf `shouldBe` someLeaf
      it "reverses order of child nodes" $ do
        mirrorNodes someParent `shouldSatisfy` isReverseSorted . getChildren
      it "doesn't re-reverse children-of-children" $ do
        getChildren (mirrorNodes someGrandParent) `shouldBe` map mirrorNodes (reverse $ getChildren someGrandParent)
