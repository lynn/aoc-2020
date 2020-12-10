import Aoc
import Data.List
import Data.Strings
import Data.SortedMap as M
%default total

Bag : Type
Bag = String

Count : Type
Count = SortedMap Bag Nat

Rule : Type
Rule = (Bag, Count)

Rules : Type
Rules = SortedMap Bag Count

parseRule : String -> Maybe Rule
parseRule s =
  case words s of
    (a::b::"bags"::"contain"::rest) =>
      let
        go : List String -> List (Bag, Nat)
        go ("no"::_) = []
        go (n::c::d::_::rest) = (c++" "++d, maybe 0 id $ parsePositive n) :: go rest
        go _ = []
      in Just (a++" "++b, M.fromList (go rest))
    _ => Nothing

mergeCount : Count -> Count -> Count
mergeCount = M.mergeWith (+)

-- This function is partial, because the input might as well contain
-- a cycle of bags containing each other, which I don't want to worry
-- about handling.
partial
totalCount : Bag -> Rules -> Count
totalCount b r =
  case lookup b r of
    Nothing => M.empty
    Just c => foldl mergeCount c [map (*n) (totalCount b' r) | (b',n) <- M.toList c]

partial
main : IO ()
main = do
  rules <- M.fromList <$> parseLines parseRule
  let totals = M.fromList [(bag, totalCount bag rules) | bag <- M.keys rules]
  let shinyGold = "shiny gold"
  putStr "*   "
  printLn $ count ((>0) . sum . M.lookup shinyGold) totals
  putStr "**  "
  printLn $ map sum $ M.lookup shinyGold totals
