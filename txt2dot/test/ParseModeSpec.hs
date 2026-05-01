module ParseModeSpec (spec) where

import Test.Hspec
import qualified Test.QuickCheck as QC
import qualified Test.Hspec.QuickCheck as HspecQC

import ParseLine (TodoLine(..))
import ParseMode
import TodoGraph

isFailure :: ParseModeState -> Bool
isFailure (Failure _) = True
isFailure _ = False

isLineMode :: ParseModeState -> Bool
isLineMode (LineMode _ _) = True
isLineMode _ = False

isBlockMode :: ParseModeState -> Bool
isBlockMode (BlockMode _ _ _) = True
isBlockMode (InitialBlockMode _ ) = True
isBlockMode _ = False

arbitraryTodoGraph :: QC.Gen TodoGraph
arbitraryTodoGraph = QC.sized $ \n -> do
  case n `compare` 0 of
    LT -> error "got a negative n in 'sized' :("
    EQ -> return $ TodoNode "" []
    GT -> do
      label <- QC.arbitrary
      numChildren <- QC.chooseInt (0, decimate n)
      children <- sequence $ replicate numChildren childSizedGraph
      return $ TodoNode label children
      where
        childSizedGraph :: QC.Gen TodoGraph
        childSizedGraph = QC.scale decimate arbitraryTodoGraph

decimate :: Int -> Int
decimate = round . sqrt . (fromIntegral :: Int -> Double) . pred

arbitraryRootList :: QC.Gen [TodoGraph]
arbitraryRootList = QC.sized $ \n -> do
  if n == 0
    then return []
    else sequence $ replicate (decimate n) $ QC.scale decimate arbitraryTodoGraph

arbitraryInitialBlockMode :: QC.Gen ParseModeState
arbitraryInitialBlockMode = do
  contextLines <- QC.arbitrary
  return $ InitialBlockMode contextLines

arbitraryLineMode :: QC.Gen ParseModeState
arbitraryLineMode = QC.sized $ \n -> do
  if n == 0
    then return $ LineMode [] $ leafNode ""
    else do
      roots <- QC.scale decimate arbitraryRootList
      current <- QC.scale pred arbitraryTodoGraph
      return $ LineMode roots current

arbitraryBlockMode :: QC.Gen ParseModeState
arbitraryBlockMode = QC.sized $ \n -> do
  if n == 0
    then return $ BlockMode [] [] $ leafNode ""
    else do
      roots <- QC.scale decimate arbitraryRootList
      blockLines <- QC.scale pred QC.arbitrary
      current <- QC.scale pred arbitraryTodoGraph
      return $ BlockMode roots blockLines current

arbitraryFailure :: QC.Gen ParseModeState
arbitraryFailure = do
  msg <- QC.arbitrary
  return $ Failure msg

newtype MyParseState = MyParseState{ unwrap :: ParseModeState }
  deriving (Show, Read, Eq)

instance QC.Arbitrary MyParseState where
  arbitrary = QC.oneof $ (map (fmap MyParseState)) [
    return InitialMode
    , arbitraryInitialBlockMode
    , arbitraryLineMode
    , arbitraryBlockMode
    , arbitraryFailure
    ]
  -- TODO: implement shrink?

whichCtor :: MyParseState -> String
whichCtor MyParseState{unwrap=InitialMode} = "InitialMode"
whichCtor MyParseState{unwrap=(InitialBlockMode _)} = "InitialBlockMode"
whichCtor MyParseState{unwrap=(LineMode _ _)} = "LineMode"
whichCtor MyParseState{unwrap=(BlockMode _ _ _)} = "BlockMode"
whichCtor MyParseState{unwrap=(Failure _ )} = "Failure"

-- 10ms == 10,000us
timeLimitUS :: Int
timeLimitUS = 10 * 1000

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

    it "accepts a root-node line" $ do
      parseModeStep st0 (Line 0 $ nodeLabel someLeaf) `shouldSatisfy` isLineMode
    it "accepts a root-node block-start" $ do
      parseModeStep st0 (SectionStart 0 "foo") `shouldSatisfy` isBlockMode
    it "accepts an orphaned block-continuation at the root as a line" $ do
      parseModeStep st0 (SectionContinue 0 "bar") `shouldSatisfy` isLineMode
    it "is unchanged by 'Skip'able input" $ do
      parseModeStep st0 Skip `shouldBe` st0

    it "rejects leading tabs" $ do
      parseModeStep st0 (Line 3 $ nodeLabel someLeaf) `shouldSatisfy` isFailure
  describe "for any state" $ do
    HspecQC.prop "is unchanged by 'Skip'able input" $
      \someState ->
        QC.collect (whichCtor someState) $
          QC.within timeLimitUS $
            (parseModeStep . unwrap) someState Skip `shouldBe` ((unwrap someState) :: ParseModeState)

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
