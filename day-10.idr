import Aoc
import Data.List
import Data.List1
import Data.SortedMap
%default total

-- TODO. I solved it in Python already shh
main : IO ()
main = do
  ns <- readIntegerLines {a=Integer}
  let sorted = sort ns
  let adapters = 0 ::: sorted
  let final = last adapters + 3
  printLn final
