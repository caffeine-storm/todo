module Main where

import Data.Maybe (fromJust)
import qualified Txt2Dot

main :: IO ()
main = do
    inputData <- readFile "/dev/stdin"
    let graph = fromJust $ Txt2Dot.parseText inputData
        asDotText = Txt2Dot.showDot graph
    writeFile "/dev/stdout" (asDotText ++ "\n")
