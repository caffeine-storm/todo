module TodoGraph where

import ParseLine (leadingTabCount)

data TodoGraph = TodoNode String [TodoGraph]
    deriving(Eq, Show, Read)

nodeLabel :: TodoGraph -> String
nodeLabel (TodoNode lbl _) = lbl

leafNode :: String -> TodoGraph
leafNode lbl = TodoNode lbl []

leafNode' :: String -> (Int, TodoGraph)
leafNode' lbl =
    let tabs = leadingTabCount lbl
    in  (tabs, TodoNode (drop tabs lbl) [])

appendChild :: TodoGraph -> TodoGraph -> TodoGraph
appendChild (TodoNode lbl kids) newKid =
    TodoNode lbl (newKid:kids)

appendDescendant :: TodoGraph -> Int -> TodoGraph -> TodoGraph
appendDescendant base 0 newNode =
  appendChild base newNode
appendDescendant (TodoNode _ []) depth _ = error $ "can't appendDescendant to depth " ++ show depth ++ " without children in the 'base' node"
appendDescendant (TodoNode lbl (leadingChild:kids)) depth newNode =
  TodoNode lbl $ appendDescendant leadingChild (pred depth) newNode:kids

graphForRootList :: [TodoGraph] -> TodoGraph
graphForRootList = TodoNode ""

blockToNode :: [String] -> TodoGraph
blockToNode = leafNode . init . unlines . reverse
