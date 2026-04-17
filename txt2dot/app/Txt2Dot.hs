module Txt2Dot where

import Data.List (intercalate, isPrefixOf)
import Data.Char (isSpace)

leadingTabCount :: String -> Int
leadingTabCount =
    length . takeWhile (== '\t')

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

data ParseState = ParseState{
  roots :: [TodoGraph],
  curTabs :: Int,
  currentBlock :: [String]
  }
    deriving(Show)

newParseState :: ParseState
newParseState = ParseState{roots = [], curTabs = -1, currentBlock = []}

isParsingBlock :: ParseState -> Bool
isParsingBlock ParseState{currentBlock=[]} = False
isParsingBlock _ = True

-- Add a new root node to the graph.
newRoot :: ParseState -> String -> ParseState
newRoot state label =
  let (tabs, newRootNode) = leafNode' label
  in  if tabs /= 0 then
        error "a root needs no leading tabs!"
      else
        state {roots = newRootNode:(roots state), curTabs=0}

addNode :: ParseState -> String -> ParseState
addNode st@ParseState{roots=[]} label =
  newRoot st label
addNode st@ParseState{roots=(r:rs)} label =
  -- TODO: handle the case where a root node also starts a 'block'/'section'
  -- TODO: handle the case where the last node is a section node
  if newDepth == 0
    then newRoot st label
    else if isParsingBlock st
      -- TODO: handle `section >> section`
      then if "  " `isPrefixOf` newLabel
        then st {currentBlock=((drop 2 newLabel):currentBlock st)}
        else finishParsingBlock
      else if "- " `isPrefixOf` newLabel
        then startParsingBlock
        else st {roots=(r':rs), curTabs=newDepth}
  where
    (newDepth, newNode) = leafNode' label
    newLabel = nodeLabel newNode
    -- drop a trailing newline from typical 'unlines'
    unlines' :: [String] -> String
    unlines' = init . unlines

    (_, sectionLeaf) = leafNode' $ unlines' $ reverse $ currentBlock st
    finishParsingBlock = st {roots=(r'':rs), currentBlock=[]}
    r' = addNode' newNode newDepth r
    -- TODO: curTabs st, instead of newDepth, I think?
    r'' = addNode' newNode newDepth (addNode' sectionLeaf (curTabs st) r)

    startParsingBlock :: ParseState
    startParsingBlock = st {currentBlock=[drop 2 newLabel]}

    addNode' :: TodoGraph -> Int -> TodoGraph -> TodoGraph
    addNode' noob 1 existingNode =
      addChild existingNode noob
    addNode' noob depth (TodoNode lbl kids) =
      let target = head $ kids
          replacement = addNode' noob (pred depth) target
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
    getGraph $ foldl parseLine newParseState $ filter (not . isNoise) $ lines input
    where
      isNoise :: String -> Bool
      isNoise = all isSpace

type NodeId = String
type LabelString = String

data ShowStateNode = ShowStateNode NodeId LabelString

escapeString :: String -> String
escapeString "" = ""
escapeString ('\\':rest) = "\\\\" ++ escapeString rest
escapeString ('"':rest) = "\\\"" ++ escapeString rest
escapeString (x:rest) = (x:escapeString rest)

quoted :: String -> String
quoted s = concat ["\"", escapeString s, "\""]

instance Show ShowStateNode where
  show (ShowStateNode nodeId label) = concat [(quoted nodeId), " [label=", quoted label, "];"]

data ShowStateEdge = ShowStateEdge NodeId NodeId
instance Show ShowStateEdge where
  show (ShowStateEdge lhs rhs) = concat [(quoted lhs), " -> ", (quoted rhs), ";"]

data ShowState = ShowState {
  nodes :: [ShowStateNode],
  edges :: [ShowStateEdge],

  -- holds a stack of Ints tracking current size of parent nodes.
  -- new level? nextId <- 0:nextId
  -- new node? (newId, nextId) <- (
  --     intercalate "_" (map show (reverse $ [succ $ head nextId] ++ nextId)),
  --     (succ $ head nextId):(tail nextId)
  -- )
  -- Always non-empty
  nextId :: [Int]
} deriving(Show)
newShowState :: ShowState
newShowState = ShowState {
  nodes = [],
  edges = [],
  nextId = [0]
}

mintNextId :: ShowState -> (NodeId, ShowState)
mintNextId ShowState{nextId = []} = error "malformed ShowState; nextId must be non-empty!"
mintNextId st@ShowState{ nextId=(used:rest) } =
  let siblingPosition = succ used
      newid = intercalate "_" $ reverse $ map show $ (siblingPosition:rest)
  in  (newid, st { nextId = siblingPosition:rest })

addLevel :: ShowState -> ShowState
addLevel st@ShowState{ nextId=idgen } =
  st { nextId = (0:idgen) }

popLevel :: ShowState -> ShowState
popLevel st@ShowState{ nextId=(_:xs:xss) } =
  st { nextId = (xs:xss) }
popLevel _ = error "can only popLevel from non-root level"

getDot :: ShowState -> String
getDot ShowState{nodes=ns, edges=es} =
  unlines [
    "digraph {",
    unlines $ map show $ reverse ns,
    unlines $ map show es,
    "}"
  ]

writeDot :: TodoGraph -> String
-- TODO: subgraphs? each root should be a subgraph, right?
writeDot g = getDot $ writeNodes [g]

writeNodes :: [TodoGraph] -> ShowState
writeNodes kids = foldl (\st -> writeNode' st 0) newShowState kids 

-- TODO: depth is not used... ?
writeNode' :: ShowState -> Int -> TodoGraph -> ShowState
writeNode' st depth (TodoNode parentLabel kids) =
  let
      (st', parentNodeId) = addNodeWithLabel st parentLabel
      st'' = addLevel st'
      st''' = foldl (\accState -> writeNode' accState (succ depth)) st'' kids
      st'''' = popLevel st'''
      st''''' = addEdges st'''' parentNodeId kids
  in  st'''''

addNodeWithLabel :: ShowState -> String -> (ShowState, NodeId)
addNodeWithLabel st lbl =
  let (nodeId, st') = mintNextId st
      newNode = ShowStateNode nodeId lbl
  in  (st' { nodes = newNode:(nodes st) }, nodeId)

addEdges :: ShowState -> NodeId -> [TodoGraph] -> ShowState
addEdges st parent kids =
  st { edges = [ShowStateEdge parent kidId | kidId <- [parent ++ "_" ++ (show n) | n <- [1 .. length kids]]] ++ (edges st) }
