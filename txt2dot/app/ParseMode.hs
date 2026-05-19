module ParseMode where

import TodoGraph
import ParseLine
import Data.Maybe (maybeToList)

-- TODO: rename to 'BlockInProgress' or smth?
data BlockParseState = BlockParseState [String] Int
  deriving(Show, Read, Eq)
newtype RootBlockParseState = RootBlockParseState [String]
  deriving(Show, Read, Eq)

data ParseModeState =
  RootLineMode [TodoGraph] |
  RootBlockMode [TodoGraph] RootBlockParseState |
  LineMode [TodoGraph] TodoGraph |
  BlockMode [TodoGraph] BlockParseState TodoGraph |
  -- TODO: not `Failure String` but `Failure ErrorMessage`
  Failure String
  deriving(Show, Read, Eq)

graphDepth :: TodoGraph -> Int
graphDepth (TodoNode _ []) = 1
graphDepth (TodoNode _ (x:_)) = 1 + graphDepth x

newParseModeState :: ParseModeState
newParseModeState = RootLineMode []

modeRoots :: ParseModeState -> [TodoGraph]
modeRoots (RootLineMode roots) = roots
modeRoots (RootBlockMode roots _) = roots
modeRoots (LineMode roots _) = roots
modeRoots (BlockMode roots _ _) = roots
modeRoots st@(Failure _) = failCase "modeRoots" st

modeCurTabs :: ParseModeState -> Int
modeCurTabs (RootLineMode _) = 0
modeCurTabs (RootBlockMode _ _) = 0
modeCurTabs (LineMode _ curNode) = graphDepth curNode
modeCurTabs (BlockMode [] _ curNode) = graphDepth curNode
modeCurTabs (BlockMode (r:_) _ _) = graphDepth r
modeCurTabs st@(Failure _) = failCase "modeCurTabs" st

modeCurrentBlock :: ParseModeState -> BlockParseState
modeCurrentBlock (RootBlockMode _ (RootBlockParseState prevLines)) = BlockParseState prevLines 0
modeCurrentBlock (BlockMode _ curBlock _) = curBlock
modeCurrentBlock st = failCase "modeCurrentBlock" st

failCase :: String -> ParseModeState -> a
failCase methodName st  = error $ "can't " ++ methodName ++ " on parse state " ++ show st

modeGetGraph :: ParseModeState -> Maybe TodoGraph
modeGetGraph (RootLineMode []) = Nothing
modeGetGraph (RootLineMode roots) = Just $ graphForRootList $ reverse roots
modeGetGraph (RootBlockMode roots (RootBlockParseState prevLines)) = modeGetGraph $ LineMode roots $ blockToNode prevLines
modeGetGraph (LineMode [] curr) = Just curr
modeGetGraph (LineMode roots curr) = Just $ graphForRootList $ reverse (curr:roots)
modeGetGraph (BlockMode roots (BlockParseState prevLines n) curr) = modeGetGraph $ LineMode roots $ appendDescendant curr n $ blockToNode prevLines
modeGetGraph (Failure _) = Nothing

addNode :: TodoGraph -> Int -> TodoGraph -> Either String TodoGraph
addNode (TodoNode lbl kids) 0 newChild = Right $ TodoNode lbl (newChild:kids)
addNode (TodoNode lbl []) 1 newChild = Right $ TodoNode lbl [newChild]
addNode (TodoNode _ []) _ _ = Left "can't recurse down a node with no children"
addNode (TodoNode lbl (kid:kids)) n newChild =
  case newHead of
    Left msg -> Left msg
    Right nd -> Right $ TodoNode lbl (nd:kids)
  where
  newHead :: Either String TodoGraph
  newHead = addNode kid (pred n) newChild

parseModeStep :: ParseModeState -> TodoLine -> ParseModeState
parseModeStep x Skip = x
parseModeStep it@(Failure _) _ = it

parseModeStep (RootLineMode roots) (Line 0 label) = LineMode roots $ leafNode label
parseModeStep (RootLineMode _) (Line n _) = Failure $ badTabs 0 n
parseModeStep (RootLineMode roots) (SectionStart 0 label) =
  RootBlockMode roots $ RootBlockParseState [label]
parseModeStep (RootLineMode _) (SectionStart n _) = Failure $ badTabs 0 n
parseModeStep (RootLineMode roots) (SectionContinue 0 label) =
  parseModeStep (RootLineMode roots) $ Line 0 $ "  " ++ label
parseModeStep (RootLineMode _) (SectionContinue n _) = Failure $ badTabs 0 n

parseModeStep (RootBlockMode roots (RootBlockParseState prevLines)) (Line 0 label) =
  LineMode (blockToNode prevLines:roots) $ leafNode label

parseModeStep (RootBlockMode roots (RootBlockParseState prevLines)) (Line 1 label) =
  let base = blockToNode prevLines
      child = leafNode label
      currNode = appendChild base child
  in  LineMode roots currNode
parseModeStep (RootBlockMode _ _) (Line n _) =
  Failure $ badTabs 0 n

parseModeStep (RootBlockMode roots (RootBlockParseState prevLines)) (SectionStart 0 label) =
  RootBlockMode (blockToNode prevLines:roots) $ RootBlockParseState [label]

parseModeStep (RootBlockMode roots (RootBlockParseState prevLines)) (SectionStart 1 label) =
  -- 'prevLines' is a block-node that will own a new block-node that we might
  -- not have seen the end of yet.
  let base = blockToNode prevLines
  in  BlockMode roots (BlockParseState [label] 1) base

parseModeStep (RootBlockMode _ _) (SectionStart n _) =
  Failure $ badTabs 0 n

parseModeStep (RootBlockMode roots (RootBlockParseState [])) (SectionContinue 0 label) =
  -- it's not a continuation; it must be interpreted as a Line that happens to
  -- have two leading spaces
  parseModeStep (RootBlockMode roots (RootBlockParseState [])) $ Line 0 $ "  " ++ label
parseModeStep (RootBlockMode roots (RootBlockParseState prevLines)) (SectionContinue 0 label) =
  -- it is a continuation; just collect more to 'prevLines'
  RootBlockMode roots $ RootBlockParseState $ label:prevLines
parseModeStep (RootBlockMode roots (RootBlockParseState prevLines)) (SectionContinue 1 label) =
  -- it's not a continuation; it's a Line that has two leading spaces _AND_ is
  -- one tab in from the root level
  if null prevLines
    then if null roots
      then Failure $ badTabs 0 1
      -- TODO: ... would we ever get here? we have roots but no context lines...
      else patchHeadRoot
    else LineMode roots $ appendChild (blockToNode prevLines) $ leafNode $ "  " ++ label
  where
    patchHeadRoot :: ParseModeState
    patchHeadRoot = case patchety of
      Right good -> LineMode (tail roots) good
      Left msg -> Failure msg
    patchety = addNode (head roots) 1 $ leafNode $ "  " ++ label

parseModeStep (RootBlockMode _ _) (SectionContinue n _) =
  Failure $ badTabs 0 n

parseModeStep (LineMode roots curr) (Line 0 label) =
  LineMode (curr:roots) $ leafNode label
parseModeStep (LineMode roots curr) (Line n label) =
  case addNode curr n $ leafNode label of
    Right result -> LineMode roots result
    Left msg -> Failure msg

-- parseModeStep (LineMode roots curr) (SectionStart _ _) = TODO
-- parseModeStep (LineMode roots curr) (SectionContinue _ _) = TODO

parseModeStep (BlockMode roots (BlockParseState prevLines blockTabs) curr) (Line n label) =
  let tabDelta = n - blockTabs
      prevBlock = blockToNode prevLines
      curr' = addNode curr blockTabs prevBlock
      lineModeResult = case curr' of
        Right good -> LineMode roots good
        Left msg -> Failure msg
      newRootResult = parseModeStep (RootLineMode (prevBlock:roots)) (Line n label)
  in if tabDelta > 1
        then Failure $ badTabs blockTabs n
        else if tabDelta < 0
          then newRootResult
          else lineModeResult

parseModeStep st@(BlockMode roots (BlockParseState _ blockTabs) curr) (SectionStart n label) =
  let curTabs = modeCurTabs st
      curr' = addNode curr blockTabs $ leafNode label
  in
    if n - curTabs > 1
      then Failure $ badTabs curTabs n
      else case curr' of
        Right good -> LineMode roots good
        Left msg -> Failure msg

-- parseModeStep st@(BlockMode roots prevLines curr) (SectionContinue n label) = TODO


-- Error messages

-- type ErrorMessage = String
badTabs :: Int -> Int -> String
badTabs expected found =
  "can't accept a line with " ++ show found ++
  " tabs at level " ++ show expected

-- entry point
parseTextModal :: String -> [TodoGraph]
parseTextModal inputText =
  let inputLines = lines inputText
      linewiseInput = map parseLine inputLines
      startState = newParseModeState
      lastState = foldl parseModeStep startState linewiseInput
  in  maybeToList $ modeGetGraph lastState
