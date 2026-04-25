module ParseModeSpec (spec) where

import Test.Hspec
import TodoGraph
import ParseLine (
  TodoLine(..)
  )
import ParseMode

someLeaf :: TodoGraph
someLeaf = leafNode "some label"

st0 :: ParseModeState
st0 = newParseModeState

spec :: Spec
spec = do
  describe "with a new state" $ do
    it "starts out empty" $ do
      modeRoots st0 `shouldBe` []
      modeGetGraph st0 `shouldBe` Nothing

    it "can take a step" $ do
      parseModeStep st0 (Line 0 $ nodeLabel someLeaf) `shouldBe` (LineMode [] someLeaf)
