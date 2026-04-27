module ParseMode where

import TodoGraph
import ParseLine

data ParseModeState =
  InitialMode |
  InitialBlockMode [String] |
  LineMode [TodoGraph] TodoGraph |
  BlockMode [TodoGraph] [String] TodoGraph |
  Failure String
  deriving(Show, Read, Eq)

graphDepth :: TodoGraph -> Int
graphDepth (TodoNode _ []) = 1
graphDepth (TodoNode _ (x:_)) = 1 + graphDepth x

newParseModeState :: ParseModeState
newParseModeState = InitialMode

modeRoots :: ParseModeState -> [TodoGraph]
modeRoots InitialMode = []
modeRoots (LineMode rootList _) = rootList
modeRoots (BlockMode rootList _ _) = rootList
modeRoots st = failCase "modeRoots" st

modeCurTabs :: ParseModeState -> Int
modeCurTabs InitialMode = -1
modeCurTabs (LineMode _ curNode) = graphDepth curNode
modeCurTabs (BlockMode (r:_) _ _) = graphDepth r
modeCurTabs st = failCase "modeCurTabs" st

modeCurrentBlock :: ParseModeState -> [String]
modeCurrentBlock (BlockMode _ curBlock _) = curBlock
modeCurrentBlock st = failCase "modeCurrentBlock" st

failCase :: String -> ParseModeState -> a
failCase methodName st  = error $ "can't " ++ methodName ++ " on parse state " ++ show st

modeGetGraph :: ParseModeState -> Maybe TodoGraph
modeGetGraph InitialMode = Nothing
modeGetGraph (Failure _) = Nothing
modeGetGraph (LineMode [] curr) = Just $ curr
modeGetGraph (LineMode roots curr) = Just $ graphForRootList $ reverse (curr:roots)

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
parseModeStep it@(Failure _) _ = it

parseModeStep InitialMode (Line 0 label) = LineMode [] $ leafNode label
parseModeStep InitialMode (Line _ _) = Failure "root nodes must start with leading tabs"
parseModeStep InitialMode (SectionStart 0 label) =
  InitialBlockMode [label]
parseModeStep InitialMode (SectionContinue 0 label) =
  parseModeStep InitialMode (Line 0 ("  " ++ label))

parseModeStep (LineMode roots curr) (Line 0 label) =
  LineMode (curr:roots) $ leafNode label
parseModeStep (LineMode roots curr) (Line n label) =
  case addNode curr n $ leafNode label of
    Left result -> LineMode roots result
    Right msg -> Failure msg
