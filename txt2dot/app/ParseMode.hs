module ParseMode where

import TodoGraph
import ParseLine

data ParseModeState =
  RootLineMode [TodoGraph] |
  RootBlockMode [TodoGraph] [String] |
  LineMode [TodoGraph] TodoGraph |
  BlockMode [TodoGraph] [String] TodoGraph |
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

modeCurrentBlock :: ParseModeState -> [String]
modeCurrentBlock (RootBlockMode _ curBlock) = curBlock
modeCurrentBlock (BlockMode _ curBlock _) = curBlock
modeCurrentBlock st = failCase "modeCurrentBlock" st

failCase :: String -> ParseModeState -> a
failCase methodName st  = error $ "can't " ++ methodName ++ " on parse state " ++ show st

modeGetGraph :: ParseModeState -> Maybe TodoGraph
modeGetGraph (RootLineMode []) = Nothing
modeGetGraph (RootLineMode roots) = Just $ graphForRootList $ reverse roots
modeGetGraph (RootBlockMode roots prevLines) = modeGetGraph $ LineMode roots $ blockToNode prevLines
modeGetGraph (LineMode [] curr) = Just $ curr
modeGetGraph (LineMode roots curr) = Just $ graphForRootList $ reverse (curr:roots)
modeGetGraph (BlockMode roots prevLines curr) = modeGetGraph $ LineMode roots $ addChild curr $ blockToNode prevLines
modeGetGraph (Failure _) = Nothing

addNode :: TodoGraph -> Int -> TodoGraph -> Either TodoGraph String
addNode (TodoNode lbl kids) 0 newChild = Left $ TodoNode lbl (newChild:kids)
addNode (TodoNode lbl []) 1 newChild = Left $ TodoNode lbl [newChild]
addNode (TodoNode _ []) _ _ = Right "can't recurse down a node with no children"
addNode (TodoNode lbl (kid:kids)) n newChild =
  case newHead of
    Right msg -> Right msg
    Left nd -> Left $ TodoNode lbl (nd:kids)
  where
  newHead :: Either TodoGraph String
  newHead = addNode kid (pred n) newChild

parseModeStep :: ParseModeState -> TodoLine -> ParseModeState
parseModeStep x Skip = x
parseModeStep it@(Failure _) _ = it

parseModeStep (RootLineMode roots) (Line 0 label) = LineMode roots $ leafNode label
parseModeStep (RootLineMode _) (Line n _) = Failure $ badTabs 0 n
parseModeStep (RootLineMode roots) (SectionStart 0 label) =
  RootBlockMode roots [label]
parseModeStep (RootLineMode roots) (SectionContinue 0 label) =
  parseModeStep (RootLineMode roots) $ Line 0 $ "  " ++ label

parseModeStep (RootBlockMode roots prevLines) (Line 0 label) =
  LineMode (blockToNode prevLines:roots) $ leafNode label
parseModeStep (RootBlockMode roots prevLines) (Line 1 label) =
  let base = blockToNode prevLines
      child = leafNode label
      currNode = addChild base child
  in  LineMode roots currNode
parseModeStep (RootBlockMode _ _) (Line n label) =
  Failure $ badTabs 0 n

parseModeStep (RootBlockMode roots prevLines) (SectionStart 0 label) =
  RootBlockMode roots (label:prevLines)

parseModeStep (RootBlockMode roots prevLines) (SectionStart 1 label) =
  -- 'prevLines' is a block-node that will own a new block-node that we might
  -- not have seen the end of yet.
  let base = blockToNode prevLines
  in  BlockMode roots [label] base

parseModeStep (RootBlockMode _ _) (SectionStart n label) =
  Failure $ badTabs 0 n

-- | SectionStart Int String
-- | SectionContinue Int String
-- | Skip


parseModeStep (LineMode roots curr) (Line 0 label) =
  LineMode (curr:roots) $ leafNode label
parseModeStep (LineMode roots curr) (Line n label) =
  case addNode curr n $ leafNode label of
    Left result -> LineMode roots result
    Right msg -> Failure msg

-- Error messages

-- type ErrorMessage = String
badTabs :: Int -> Int -> String
badTabs expected found =
  "can't accept a line with " ++ (show found) ++
  " tabs at level " ++ (show expected)
