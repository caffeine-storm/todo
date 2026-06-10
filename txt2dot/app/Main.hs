module Main where

import qualified ParseMode
import qualified Txt2Dot

main :: IO ()
main = do
    inputData <- readFile "/dev/stdin"
    let graphs = case ParseMode.parseTextModal inputData of
                    Nothing -> error "couldn't parse input as graph"
                    Just graphs -> graphs
        asDotText = Txt2Dot.writeDot' graphs
    writeFile "/dev/stdout" (asDotText ++ "\n")
