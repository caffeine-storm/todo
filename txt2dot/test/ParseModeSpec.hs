module ParseModeSpec (spec) where

import Test.Hspec
import TodoGraph
import ParseLine (
  TodoLine(..)
  )
import ParseMode

isFailure :: ParseModeState -> Bool
isFailure (Failure _) = True
isFailure _ = False

spec :: Spec
spec = do
  let st0 = newParseModeState
      someLeaf = leafNode "some label"
      st1 = LineMode [] someLeaf
  describe "with a new state" $ do
    it "starts out empty" $ do
      modeRoots st0 `shouldBe` []
      modeGetGraph st0 `shouldBe` Nothing

    it "can take a step" $ do
      parseModeStep st0 (Line 0 $ nodeLabel someLeaf) `shouldBe` st1

    it "rejects leading tabs" $ do
      parseModeStep st0 (Line 3 $ nodeLabel someLeaf) `shouldSatisfy` isFailure
