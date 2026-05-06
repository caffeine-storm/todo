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

graphForRootList :: [TodoGraph] -> TodoGraph
graphForRootList roots = TodoNode "" roots

blockToNode :: [String] -> TodoGraph
blockToNode = leafNode . init . unlines . reverse
