module WriteSpec (spec) where

import Data.List (intercalate, isPrefixOf)
import Test.Hspec

import Txt2Dot (writeDot, TodoGraph(..), ShowStateNode(..), newShowState, quoted, mintNextId, addLevel)

-- ("b-node" [])
bNode :: TodoGraph
bNode = TodoNode "b-node" []

-- ("1" [])
n1 :: TodoGraph
n1 = TodoNode "1" []

-- ("2" [])
n2 :: TodoGraph
n2 = TodoNode "2" []

-- ("3" [])
n3 :: TodoGraph
n3 = TodoNode "3" []

success :: Expectation
success = return ()

-- Asserts that the first list has, as sublists in the same order, the elements
-- of the second list.
shouldContainSubsequences :: Eq a => [a] -> [[a]] -> Expectation
-- No more needles? Success!
shouldContainSubsequences _ [] = success
-- Next needle is empty? Check the same haystack for the rest of the needles
shouldContainSubsequences haystack ([]:ns) = shouldContainSubsequences haystack ns
-- No more haystack? We didn't match no-needles case, so we haven't found
-- everything :(
shouldContainSubsequences [] _ = expectationFailure "couldn't find all subsequences"
shouldContainSubsequences haystack (n:ns) =
  if n `isPrefixOf` haystack then
    -- If the haystack starts with the needle, check the rest of the stack
    -- against the rest of the needles.
    shouldContainSubsequences (drop (length n) haystack) ns
  else
    -- If the haystack doesn't start with the next needle, chop the first
    -- element off the front of the haystack and try again.
    shouldContainSubsequences (tail haystack) (n:ns)

labelFor :: TodoGraph -> String
labelFor (TodoNode lbl _) = "label=" ++ (quoted lbl)

nodeIdFor :: [Int] -> String
nodeIdFor posStack =
  intercalate "_" $ reverse $ map show posStack

edgeFor :: [Int] -> String
edgeFor posStack =
  -- just the edge for child at front of posStack
  let child = nodeIdFor posStack
      parent = nodeIdFor $ tail posStack
  in  concat [
        quoted parent,
        " -> ",
        quoted child,
        ";"
      ]

spec :: Spec
spec = do
  describe "node ids" $ do
    it "should be quoted when shown" $ do
      (show $ ShowStateNode (nodeIdFor [1]) "some-label") `shouldContain` (quoted $ nodeIdFor [1])
    it "should mint the right ids" $ do
      let st0 = newShowState
          (id1, st1) = mintNextId st0
          st2        = addLevel st1
          (id3, st3) = mintNextId st2
          (id4, st4) = mintNextId st3
          (id5, _) = mintNextId st4
      id1 `shouldBe` "1"
      id3 `shouldBe` "1_1"
      id4 `shouldBe` "1_2"
      id5 `shouldBe` "1_3"
  
  describe "writing" $ do
    it "can write an empty graph" $ do
      (writeDot (TodoNode "" [])) `shouldContain` "digraph"
    it "can write a one-node graph" $ do
      (writeDot (TodoNode "i-am-a-root" [])) `shouldContain` "i-am-a-root"
    it "can write (a-node [b-node])" $ do
      let tut = writeDot (TodoNode "a-node" [bNode])
      tut `shouldContain` "\"a-node\""
      tut `shouldContain` "\"b-node\""
      tut `shouldContain` (nodeIdFor [1])
      tut `shouldContain` (nodeIdFor [1, 1])
      tut `shouldContain` (edgeFor [1, 1])
    it "can write labels that have quotes" $ do
      let tut = writeDot (TodoNode "i-have-\"-a-quote" [bNode])
      tut `shouldContain` "\\\""
    it "can write (\"\" [1 2 3])" $ do
      let tut = writeDot (TodoNode "" [n1, n2, n3])
      tut `shouldNotContain` "1_1_1_1"
    it "puts node declarations in order with the input graph" $ do
      let tut = writeDot (TodoNode "" [n1, n2, n3])
      tut `shouldContainSubsequences` [labelFor n1, labelFor n2, labelFor n3]
      
      
