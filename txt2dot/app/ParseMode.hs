module ParseMode where

import TodoGraph
import ParseLine

data ParseModeState =
  InitialMode |
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

parseModeStep :: ParseModeState -> TodoLine -> ParseModeState
parseModeStep InitialMode (Line 0 label) = LineMode [] $ leafNode label
parseModeStep InitialMode (Line _ _) = Failure "root nodes must start with leading tabs"
parseModeStep it@(Failure _) _ = it
