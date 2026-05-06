module Txt2Dot where

import Data.List (intercalate, isPrefixOf)
import Data.Char (isSpace)

import ParseLine (leadingTabCount)
import TodoGraph

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

-- Consume a line of input by adding it as a new root node to the graph.
-- TODO: maybe don't call this fn 'newRoot' and handle sub-items here?
newRoot :: ParseState -> String -> ParseState

newRoot _ "" = error "a root needs a label"

newRoot st@ParseState{} label@('\t':_) =
  -- finish the 'currentBlock' and add a sub-node for label
  if isParsingBlock st
    then
      let oldRoots = roots st
          blockNode = blockToNode (currentBlock st)
      in  addNode (st {roots=(blockNode:oldRoots), curTabs=0, currentBlock=[]}) label
    else
      error "a root needs no leading tabs"

-- Handle erroneous block-continuation as start of root node
newRoot ParseState{currentBlock=[]} (' ':' ':_) =
  error "block-continuation can't start a node"

-- Handle start-of-block as root node
newRoot state@ParseState{currentBlock=[]} ('-':' ':label) =
  state {currentBlock=[label], curTabs=0}

-- Handle start-of-another-block as root node
newRoot state@ParseState{currentBlock=block} ('-':' ':label) =
  state {roots=(blockToNode block):(roots state), currentBlock=[label], curTabs=0}

-- Handle block-continuation as part of root node
newRoot state@ParseState{currentBlock=block} (' ':' ':label) =
  state {currentBlock=(label:block), curTabs=0}

-- Handle line as root node
newRoot state label =
  let (tabs, newRootNode) = leafNode' label
  in  if tabs /= 0 then
        error "a root needs no leading tabs!"
      else
        if isParsingBlock state
          then let blockNode = blockToNode (currentBlock state)
               in  state {roots = newRootNode:blockNode:(roots state), curTabs=0, currentBlock=[]}
          else state {roots = newRootNode:(roots state), curTabs=0}

addNode :: ParseState -> String -> ParseState
addNode st@ParseState{roots=[]} label =
  newRoot st label
addNode st@ParseState{roots=(r:rs)} label =
  if newDepth == 0
    then newRoot st label
    else if isParsingBlock st
      then if "  " `isPrefixOf` newLabel
        then st {currentBlock=((drop 2 newLabel):currentBlock st)}
        else finishParsingBlock'
      else if "- " `isPrefixOf` newLabel
        then startParsingBlock'
        else st {roots=(r':rs), curTabs=newDepth}
  where
    (newDepth, newNode) = leafNode' label
    newLabel = nodeLabel newNode
    -- drop a trailing newline from typical 'unlines'
    unlines' :: [String] -> String
    unlines' = init . unlines

    (_, sectionLeaf) = leafNode' $ unlines' $ reverse $ currentBlock st
    finishParsingBlock' = st {roots=(r'':rs), currentBlock=[]}
    r' = addNode' newNode newDepth r
    -- TODO: curTabs st, instead of newDepth, I think?
    r'' = addNode' newNode newDepth (addNode' sectionLeaf (curTabs st) r)

    startParsingBlock' :: ParseState
    startParsingBlock' = st {currentBlock=[drop 2 newLabel]}

    addNode' :: TodoGraph -> Int -> TodoGraph -> TodoGraph
    addNode' noob 1 existingNode =
      appendChild existingNode noob
    addNode' noob depth (TodoNode lbl kids) =
      let target = head $ kids
          replacement = addNode' noob (pred depth) target
      in  TodoNode lbl (replacement:(tail kids))

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
  let lastState = foldl parseLine newParseState $ filter (not . isNoise) $ lines input
      finalState = if isParsingBlock lastState
          then addNode (lastState {currentBlock=[]}) $ nodeLabel $ blockToNode $ currentBlock lastState
          else lastState
  in  getGraph finalState
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
