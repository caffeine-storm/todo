module TodoGraph where

import ParseLine (leadingTabCount)

data TodoGraph = TodoNode String [TodoGraph]
    deriving(Eq, Show, Read)

getChildren :: TodoGraph -> [TodoGraph]
getChildren (TodoNode _ kids) = kids

getLabel :: TodoGraph -> String
getLabel (TodoNode lbl _) = lbl

leafNode :: String -> TodoGraph
leafNode lbl = TodoNode lbl []

leafNode' :: String -> (Int, TodoGraph)
leafNode' lbl =
    let tabs = leadingTabCount lbl
    in  (tabs, TodoNode (drop tabs lbl) [])

leadingEdgeDepth :: TodoGraph -> Int
leadingEdgeDepth (TodoNode _ []) = 1
leadingEdgeDepth (TodoNode _ (x:_)) = 1 + leadingEdgeDepth x

appendChild :: TodoGraph -> TodoGraph -> TodoGraph
appendChild (TodoNode lbl kids) newKid =
    TodoNode lbl (newKid:kids)

appendDescendant :: TodoGraph -> Int -> TodoGraph -> TodoGraph
appendDescendant base 0 newNode =
  appendChild base newNode
appendDescendant (TodoNode _ []) depth _ = error $ "can't appendDescendant to depth " ++ show depth ++ " without children in the 'base' node"
appendDescendant (TodoNode lbl (leadingChild:kids)) depth newNode =
  TodoNode lbl $ appendDescendant leadingChild (pred depth) newNode:kids

blockToNode :: [String] -> TodoGraph
blockToNode = leafNode . init . unlines . reverse

mirrorNodes :: TodoGraph -> TodoGraph
mirrorNodes (TodoNode lbl kids) = TodoNode lbl $ reverse $ map mirrorNodes kids
