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

isLineMode :: ParseModeState -> Bool
isLineMode (LineMode _ _) = True
isLineMode _ = False

spec :: Spec
spec = do
  let st0 = newParseModeState
      someLeaf = leafNode "some label"
      st1 = LineMode [] someLeaf
      lineModeState = st1
  describe "addNode" $ do
    it "can add a new level" $ do
      addNode (TodoNode "foo" []) 1 (leafNode "bar") `shouldBe` (Left (TodoNode "foo" [TodoNode "bar" []]))
  describe "with a new state" $ do
    it "starts out empty" $ do
      modeRoots st0 `shouldBe` []
      modeGetGraph st0 `shouldBe` Nothing

    it "can take a step" $ do
      parseModeStep st0 (Line 0 $ nodeLabel someLeaf) `shouldSatisfy` isLineMode

    it "rejects leading tabs" $ do
      parseModeStep st0 (Line 3 $ nodeLabel someLeaf) `shouldSatisfy` isFailure

  describe "in LineMode" $ do
    it "can yield a graph" $ do
      modeGetGraph lineModeState `shouldNotBe` Nothing
    describe "it can read more" $ do
      it "as a sibling line" $ do
        parseModeStep lineModeState (Line 0 "sibling") `shouldSatisfy` isLineMode
      it "as a subline" $ do
        parseModeStep lineModeState (Line 1 "child") `shouldSatisfy` isLineMode
      it "rejects too deep of a line" $ do
        parseModeStep lineModeState (Line 2 "too deep") `shouldNotSatisfy` isLineMode
