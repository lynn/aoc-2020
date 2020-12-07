import Aoc
import Data.List
import Data.Strings
import Data.SortedMap as M

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
        go [] = []
        go ("no"::_) = []
        go (n::c::d::_::rest) = (c++" "++d, maybe 0 id $ parsePositive n) :: go rest
      in Just (a++" "++b, M.fromList (go rest))
    _ => Nothing

mergeCount : Count -> Count -> Count
mergeCount = M.mergeWith (+)

totalCount : Bag -> Rules -> Count
totalCount b r =
  case lookup b r of
    Nothing => M.empty
    Just c => foldl mergeCount c [map (*v) (totalCount k r) | (k,v) <- M.toList c]

main : IO ()
main = do
  ps <- parseLines parseRule
  let r = M.fromList ps
  let totals = M.fromList [(k, totalCount k r) | (k, _) <- M.toList r]
  let shinyGold = "shiny gold"
  putStr "*   "
  printLn $ length [k | (k, v) <- M.toList totals, sum (M.lookup shinyGold v) > 0]
  putStr "**  "
  printLn $ map sum $ M.lookup shinyGold totals
