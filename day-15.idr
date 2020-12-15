import Aoc
import Data.List
import Data.SortedMap
import Data.Strings
%default total

play : List Int -> Nat -> Int
play input finalTurn =
    let n = length input
        d0 = fromList $ zip input [1..cast n]
    in go d0 (cast n+1) 0 (minus finalTurn (n+1))
  where
    ||| We're on turn `t`, and its number is `n`.
    ||| `d` maps numbers to the last turn they were said.
    ||| The game ends `fuel` turns from now.
    go : SortedMap Int Int -> (turn : Int) -> (num : Int) -> (fuel : Nat) -> Int
    go d t n Z = n
    go d t n (S k) = go (insert n t d) (t+1) (maybe 0 (t-) (lookup n d)) k

main : IO ()
main = do
  input <- parseLines parseInteger
  putStr "*   "; printLn (play input 2020)
  putStr "**  "; printLn (play input 30000000)
