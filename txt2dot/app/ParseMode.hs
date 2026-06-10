module ParseMode where

import TodoGraph
import ParseLine

-- TODO: rename to 'BlockInProgress' or smth?
data BlockParseState = BlockParseState [String] Int
  deriving(Show, Read, Eq)
newtype RootBlockParseState = RootBlockParseState [String]
  deriving(Show, Read, Eq)

data ParseModeState =
  RootLineMode |
  RootBlockMode RootBlockParseState |
  LineMode [TodoGraph] TodoGraph |
  BlockMode [TodoGraph] BlockParseState TodoGraph |
  -- TODO: not `Failure String` but `Failure ErrorMessage`
  Failure String
  deriving(Show, Read, Eq)

newParseModeState :: ParseModeState
newParseModeState = RootLineMode

modeRoots :: ParseModeState -> [TodoGraph]
modeRoots RootLineMode = []
modeRoots (RootBlockMode _) = []
modeRoots (LineMode roots _) = roots
modeRoots (BlockMode roots _ _) = roots
modeRoots st@(Failure _) = failCase "modeRoots" st

-- TODO: having modeCurTabs return 1 in LineMode with a singleton 'curr-node'
-- leads to sadness
modeCurTabs :: ParseModeState -> Int
modeCurTabs RootLineMode = 0
modeCurTabs (RootBlockMode _) = 0
modeCurTabs (LineMode _ curNode) = leadingEdgeDepth curNode - 1
modeCurTabs (BlockMode _ (BlockParseState _ tabs) _) = tabs
modeCurTabs st@(Failure _) = failCase "modeCurTabs" st

modeCurrentBlock :: ParseModeState -> BlockParseState
modeCurrentBlock (RootBlockMode (RootBlockParseState prevLines)) = BlockParseState prevLines 0
modeCurrentBlock (BlockMode _ curBlock _) = curBlock
modeCurrentBlock st = failCase "modeCurrentBlock" st

failCase :: String -> ParseModeState -> a
failCase methodName st  = error $ "can't " ++ methodName ++ " on parse state " ++ show st

modeGetGraph :: ParseModeState -> Maybe [TodoGraph]
modeGetGraph RootLineMode = Just []
modeGetGraph (RootBlockMode (RootBlockParseState prevLines)) = modeGetGraph $ LineMode [] $ blockToNode prevLines
modeGetGraph (LineMode roots curr) = Just $ map mirrorNodes $ reverse (curr:roots)
modeGetGraph (BlockMode roots (BlockParseState prevLines n) curr) = modeGetGraph $
  -- We must consider the block-in-progress as done now
  if n == 0
    -- The block-in-progress is its own root node
    then LineMode (curr:roots) blockNode
    -- The block-in-progress belongs to 'curr'
    else LineMode roots $ appendDescendant curr n blockNode
  where
  blockNode = blockToNode prevLines
modeGetGraph (Failure _) = Nothing

addNode :: TodoGraph -> Int -> TodoGraph -> Either String TodoGraph
addNode (TodoNode _ _) 0 replacement = Right replacement
addNode (TodoNode lbl kids) 1 newChild = Right $ TodoNode lbl (newChild:kids)
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

parseModeStep RootLineMode (Line 0 label) = LineMode [] $ leafNode label
parseModeStep RootLineMode (Line n _) = Failure $ badTabs 0 n
parseModeStep RootLineMode (SectionStart 0 label) =
  RootBlockMode $ RootBlockParseState [label]
parseModeStep RootLineMode (SectionStart n _) = Failure $ badTabs 0 n
parseModeStep RootLineMode (SectionContinue 0 label) =
  parseModeStep RootLineMode $ Line 0 $ "  " ++ label
parseModeStep RootLineMode (SectionContinue n _) = Failure $ badTabs 0 n

parseModeStep (RootBlockMode (RootBlockParseState prevLines)) (Line 0 label) =
  LineMode [blockToNode prevLines] $ leafNode label

parseModeStep (RootBlockMode (RootBlockParseState prevLines)) (Line 1 label) =
  let base = blockToNode prevLines
      child = leafNode label
      currNode = appendChild base child
  in  LineMode [] currNode
parseModeStep (RootBlockMode _) (Line n _) =
  Failure $ badTabs 0 n

parseModeStep (RootBlockMode (RootBlockParseState prevLines)) (SectionStart n label) = parseModeStep (LineMode [] (blockToNode prevLines)) (SectionStart n label)

parseModeStep (RootBlockMode (RootBlockParseState [])) (SectionContinue 0 label) =
  -- it's not a continuation; it must be interpreted as a Line that happens to
  -- have two leading spaces
  parseModeStep (RootBlockMode (RootBlockParseState [])) $ Line 0 $ "  " ++ label
parseModeStep (RootBlockMode (RootBlockParseState prevLines)) (SectionContinue 0 label) =
  -- it is a continuation; just collect more to 'prevLines'
  RootBlockMode $ RootBlockParseState $ label:prevLines
parseModeStep (RootBlockMode (RootBlockParseState prevLines)) (SectionContinue 1 label) =
  -- it's not a continuation; it's a Line that has two leading spaces _AND_ is
  -- one tab in from the root level
  if null prevLines
    then Failure $ badTabs 0 1
    else LineMode [] $ appendChild (blockToNode prevLines) $ leafNode $ "  " ++ label

parseModeStep (RootBlockMode _) (SectionContinue n _) =
  Failure $ badTabs 0 n

parseModeStep (LineMode roots curr) (Line 0 label) =
  LineMode (curr:roots) $ leafNode label
parseModeStep (LineMode roots curr) (Line n label) =
  case addNode curr n $ leafNode label of
    Right result -> LineMode roots result
    Left msg -> Failure msg
parseModeStep st@(LineMode roots curr) (SectionStart n label) =
  if n - modeCurTabs st > 1
    then Failure $ badTabs (modeCurTabs st) n
    else BlockMode roots (BlockParseState [label] n) curr
parseModeStep st@(LineMode _ _) (SectionContinue n label) =
  if n - modeCurTabs st > 1
    then Failure $ badTabs (modeCurTabs st) n
    else parseModeStep st (Line n $ "  " ++ label)

parseModeStep (BlockMode roots (BlockParseState prevLines blockTabs) curr) (Line n label) =
  let prevBlock = blockToNode prevLines
      curr' = addNode curr blockTabs prevBlock
      asLineMode = case curr' of
        Right good -> LineMode roots good
        Left msg -> Failure msg
  in  parseModeStep asLineMode (Line n label)

parseModeStep st@(BlockMode roots (BlockParseState prevLines blockTabs) curr) (SectionStart n label) =
  let curTabs = modeCurTabs st
      curr' = addNode curr blockTabs $ blockToNode prevLines
  in
    if n - curTabs > 1
      then Failure $ badTabs curTabs n
      else case curr' of
        -- Right good -> LineMode roots good
        Right good -> BlockMode roots (BlockParseState [label] n) good
        Left msg -> Failure msg

parseModeStep st@(BlockMode roots (BlockParseState labels nn) cur) (SectionContinue n label)
  | n - nn > 1 = Failure $ badTabs nn n
  -- typical block-continuation
  | n == nn = BlockMode roots (BlockParseState (label:labels) nn) cur
  -- allow a line with double-leading spaces as a new node at whatever different level
  | otherwise = parseModeStep st $ Line n $ "  " ++ label

-- Error messages

-- type ErrorMessage = String
badTabs :: Int -> Int -> String
badTabs expected found =
  "can't accept a line with " ++ show found ++
  " tabs at level " ++ show expected

-- entry point
-- returns 'Nothing' on failure, a list of root nodes on success. Can return
-- 'Just []' if there was no real data given.
parseTextModal :: String -> Maybe [TodoGraph]
parseTextModal inputText =
  let inputLines = lines inputText
      linewiseInput = map parseLine inputLines
      startState = newParseModeState
      lastState = foldl parseModeStep startState linewiseInput
  in  modeGetGraph lastState
