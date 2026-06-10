module ParseModeSpec (spec) where

import Test.Hspec
import qualified Test.QuickCheck as QC
import qualified Test.Hspec.QuickCheck as HspecQC

import ParseLine (TodoLine(..))
import ParseMode
import TodoGraph
import Control.Monad(replicateM)

isFailure :: ParseModeState -> Bool
isFailure (Failure _) = True
isFailure _ = False

isLineMode :: ParseModeState -> Bool
isLineMode (LineMode _ _) = True
isLineMode _ = False

isBlockMode :: ParseModeState -> Bool
isBlockMode (BlockMode {}) = True
isBlockMode (RootBlockMode _) = True
isBlockMode _ = False

nonNegative :: Int -> Int
nonNegative = max 0

decimate :: Int -> Int
decimate = round . sqrt . (fromIntegral :: Int -> Double) . nonNegative . pred

arbitraryTodoGraph :: QC.Gen TodoGraph
arbitraryTodoGraph = QC.sized $ \n -> do
  case n `compare` 0 of
    LT -> error "got a negative n in 'sized' :("
    EQ -> return $ TodoNode "" []
    GT -> do
      label <- QC.arbitrary
      numChildren <- QC.chooseInt (0, decimate n)
      children <- replicateM numChildren childSizedGraph
      return $ TodoNode label children
      where
        childSizedGraph :: QC.Gen TodoGraph
        childSizedGraph = QC.scale decimate arbitraryTodoGraph

arbitraryRootList :: QC.Gen [TodoGraph]
arbitraryRootList = QC.sized $ \n -> do
  replicateM (decimate n) $ QC.scale decimate arbitraryTodoGraph

arbitraryRootBlockMode :: QC.Gen ParseModeState
arbitraryRootBlockMode =
  RootBlockMode . RootBlockParseState <$> QC.arbitrary

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
    then return $ BlockMode [] (BlockParseState [] 0) $ leafNode ""
    else do
      roots <- QC.scale decimate arbitraryRootList
      blockLines <- QC.scale pred QC.arbitrary
      current <- QC.scale pred arbitraryTodoGraph
      tabCount <- QC.arbitrary
      return $ BlockMode roots (BlockParseState blockLines tabCount) current

arbitraryFailure :: QC.Gen ParseModeState
arbitraryFailure = fmap Failure QC.arbitrary

arbitraryRootLineMode :: QC.Gen ParseModeState
arbitraryRootLineMode = return RootLineMode

newtype MyParseState = MyParseState {unwrap :: ParseModeState}
  deriving (Show, Read, Eq)

instance QC.Arbitrary MyParseState where
  arbitrary =
    QC.oneof $
      map
        (fmap MyParseState)
        [ arbitraryRootLineMode,
          arbitraryRootBlockMode,
          arbitraryLineMode,
          arbitraryBlockMode,
          arbitraryFailure
        ]

-- TODO: implement shrink?

whichCtor :: MyParseState -> String
whichCtor MyParseState {unwrap = RootLineMode} = "RootLineMode"
whichCtor MyParseState {unwrap = (RootBlockMode _)} = "RootBlockMode"
whichCtor MyParseState {unwrap = (LineMode _ _)} = "LineMode"
whichCtor MyParseState {unwrap = (BlockMode {})} = "BlockMode"
whichCtor MyParseState {unwrap = (Failure _)} = "Failure"

-- 10ms == 10,000us
timeLimitUS :: Int
timeLimitUS = 10 * 1000

spec :: Spec
spec = do
  let st0 = newParseModeState
      someLeaf = leafNode "some label"
      rootLineModeState = RootLineMode
      lineModeState = LineMode [someLeaf] someLeaf
      rootBlockModeState = RootBlockMode (RootBlockParseState ["block states need context lines"])
      blockModeState = BlockMode [someLeaf] (BlockParseState ["block states need context lines"] 0) someLeaf
  describe "addNode" $ do
    it "can 'replace' the given node" $ do
      addNode (TodoNode "foo" []) 0 (leafNode "bar") `shouldBe` Right (TodoNode "bar" [])
    it "can add a new level" $ do
      addNode (TodoNode "foo" []) 1 (leafNode "bar") `shouldBe` Right (TodoNode "foo" [TodoNode "bar" []])

  describe "with a new state" $ do
    it "starts out empty" $ do
      modeRoots st0 `shouldBe` []
      modeGetGraph st0 `shouldBe` Just []

    it "accepts a root-node line" $ do
      parseModeStep st0 (Line 0 $ getLabel someLeaf) `shouldSatisfy` isLineMode
    it "accepts a root-node block-start" $ do
      parseModeStep st0 (SectionStart 0 "foo") `shouldSatisfy` isBlockMode
    it "accepts an orphaned block-continuation at the root as a line" $ do
      parseModeStep st0 (SectionContinue 0 "bar") `shouldSatisfy` isLineMode
    it "is unchanged by 'Skip'able input" $ do
      parseModeStep st0 Skip `shouldBe` st0

    it "rejects leading tabs" $ do
      parseModeStep st0 (Line 3 $ getLabel someLeaf) `shouldSatisfy` isFailure
  describe "for any state" $ do
    HspecQC.prop "is unchanged by 'Skip'able input" $
      \someState ->
        QC.collect (whichCtor someState) $
          QC.within timeLimitUS $
            (parseModeStep . unwrap) someState Skip `shouldBe` (unwrap someState :: ParseModeState)

  describe "in RootLineMode" $ do
    it "will yield 'no-graph'" $ do
      modeGetGraph rootLineModeState `shouldBe` Just []
    describe "it can read more" $ do
      it "as a sibling line" $ do
        parseModeStep rootLineModeState (Line 0 "sibling") `shouldSatisfy` isLineMode
      it "rejects a subline" $ do
        parseModeStep rootLineModeState (Line 1 "child") `shouldSatisfy` isFailure

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

  describe "in RootBlockMdoe" $ do
    it "can yield a graph" $ do
      modeGetGraph rootBlockModeState `shouldNotBe` Nothing
    describe "it can read more" $ do
      it "as a sibling line" $ do
        parseModeStep rootBlockModeState (Line 0 "sibling") `shouldSatisfy` isLineMode
      it "as a subline" $ do
        parseModeStep rootBlockModeState (Line 1 "child") `shouldSatisfy` isLineMode
      it "rejects too deep of a line" $ do
        parseModeStep rootBlockModeState (Line 2 "too deep") `shouldNotSatisfy` isLineMode

  describe "in BlockMode" $ do
    it "can yield a graph" $ do
      modeGetGraph blockModeState `shouldNotBe` Nothing
    describe "it can read more" $ do
      it "as a sibling line" $ do
        parseModeStep blockModeState (Line 0 "sibling") `shouldSatisfy` isLineMode
      it "as a subline" $ do
        parseModeStep blockModeState (Line 1 "child") `shouldSatisfy` isLineMode
      it "rejects too deep of a line" $ do
        parseModeStep blockModeState (Line 2 "too deep") `shouldNotSatisfy` isLineMode

  describe "parseTextModal" $ do
    it "can handle an empty input stream" $ do
      parseTextModal "\n" `shouldBe` Just []
    it "can handle the old, problematic input" $ do
      parseTextModal "foo\n\t- bar\n\t\tbaz" `shouldNotSatisfy` null
