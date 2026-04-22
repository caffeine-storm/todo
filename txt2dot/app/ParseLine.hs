module ParseLine where

import Data.Char (isSpace)
import Data.List (isPrefixOf)

data TodoLine =
    Line Int String
  | SectionStart Int String
  | SectionContinue Int String
  | Skip
  deriving(Show, Read, Eq)

leadingTabCount :: String -> Int
leadingTabCount =
    length . takeWhile (== '\t')

parseLine :: String -> TodoLine
parseLine "" = Skip
parseLine s =
  if all isSpace s
    then Skip
    else classify
  where
    tabs = leadingTabCount s
    payload = drop tabs s
    isSectionStart = "- " `isPrefixOf` payload
    isSectionContinue = "  " `isPrefixOf` payload
    classify = case (isSectionStart, isSectionContinue) of
      (True, _) -> SectionStart tabs (drop 2 payload)
      (_, True) -> SectionContinue tabs (drop 2 payload)
      _ -> Line tabs payload
