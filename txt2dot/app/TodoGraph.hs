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

addChild :: TodoGraph -> TodoGraph -> TodoGraph
addChild (TodoNode lbl kids) newKid =
    TodoNode lbl (newKid:kids)
