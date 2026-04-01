module Txt2Dot where

leadingTabCount :: String -> Int
leadingTabCount =
    length . takeWhile (== '\t')

data TodoGraph = TodoNode String [TodoGraph]
    deriving(Eq, Show, Read)

leafNode :: String -> TodoGraph
leafNode lbl = TodoNode lbl []

leafNode' :: String -> (Int, TodoGraph)
leafNode' lbl =
    let tabs = leadingTabCount lbl
    in  (tabs, TodoNode (drop tabs lbl) [])

addChild :: TodoGraph -> TodoGraph -> TodoGraph
addChild (TodoNode lbl kids) newKid =
    TodoNode lbl (newKid:kids)

data ParseState = ParseState{roots :: [TodoGraph], curTabs :: Int}
    deriving(Show)
newParseState :: ParseState
newParseState = ParseState{roots = [], curTabs = -1}

-- Add a new root node to the graph.
newRoot :: ParseState -> String -> ParseState
newRoot state label =
    let (tabs, newRootNode) = leafNode' label
    in  if tabs /= 0 then
            error "a root needs no leading tabs!"
        else
            state {roots = newRootNode:(roots state), curTabs=0}

-- Add a new node as a subnode to the last-parsed node.
-- TODO: look at number of tabs to know how far down the tree to go.
{-
subnode :: ParseState -> String -> ParseState
subnode st@ParseState{roots=[]} label =
    newRoot st label
subnode st@ParseState{roots=roots, curTabs=curTabs} label =
    let parent@(TodoNode lbl kids) = lastNode st
        (tabs, newNode) = leafNode' label
        replacement = parent `addChild` newNode
    in st { roots = replacement:(tail $ roots), curTabs = tabs }
-}

-- Add a sibling node to the node that was last added to the graph.
-- TODO: use leading tab count to see how far down to go.
{-
sibling :: ParseState -> String -> ParseState
sibling st label =
    st { roots = newRoots }
    where
        (x:xs) = roots st
        newRoots = (x `addChild` (leafNode label):xs)
-}

addNode :: ParseState -> String -> ParseState
addNode st@ParseState{roots=[]} label =
    newRoot st label
addNode st@ParseState{roots=(r:rs)} label =
    let r' = addNode' newDepth r
    in  if newDepth == 0 then
          newRoot st label
        else
          st {roots=(r':rs), curTabs=newDepth}
    where
        (newDepth, newNode) = leafNode' label
        addNode' :: Int -> TodoGraph -> TodoGraph
        addNode' depth existingNode@(TodoNode lbl kids) =
          if depth == 1 then
            addChild existingNode newNode
          else let target = head $ kids
                   replacement = addNode' (pred depth) target
               in  TodoNode lbl (replacement:(tail kids))

lastNode' :: TodoGraph -> TodoGraph
lastNode' it@(TodoNode _ []) = it
lastNode' (TodoNode _ (k:_)) = lastNode' k

lastNode :: ParseState -> TodoGraph
lastNode ParseState{roots=[]} = error "no node added yet T_T"
lastNode ParseState{roots=(x:_)} = lastNode' x

reverseAll :: [TodoGraph] -> [TodoGraph]
reverseAll lst =
  map revNode $ reverse lst
  where
    revNode :: TodoGraph -> TodoGraph
    revNode (TodoNode lbl kids) = TodoNode lbl (map revNode $ reverse kids)

getGraph :: ParseState -> Maybe TodoGraph
getGraph ParseState{roots=[]} = Nothing
getGraph ParseState{roots=[single]} = Just $ head $ reverseAll [single]
getGraph ParseState{roots=many} = Just $ TodoNode "" (reverseAll many)

debugShow :: String -> String
debugShow "" = ""
debugShow ('\t':cs) = "\\t" ++ debugShow cs
debugShow (c:cs) = [c] ++ debugShow cs

syntaxError :: String -> Int -> Int -> a
syntaxError line lineTabs ctxTabs =
    error $
        "bad syntax; line had depth " ++ (show lineTabs) ++
        " in a context of depth " ++ (show ctxTabs) ++
        "\n" ++ (debugShow line)

parseLine :: ParseState -> String -> ParseState
parseLine st line =
    let pastIndent = curTabs st
        currIndent = leadingTabCount line
        delta = currIndent - pastIndent
    in  if delta > 1 then syntaxError line currIndent pastIndent
        else addNode st line

parseText :: String -> Maybe TodoGraph
parseText input =
    getGraph $ foldl parseLine newParseState $ lines input

data ShowState = ShowState
newShowState :: ShowState
newShowState = ShowState

-- TODO: use ShowState for easier showing
showDot :: TodoGraph -> String
showDot it@(TodoNode _ _) =
    show it

