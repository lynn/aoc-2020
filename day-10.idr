import Aoc
import Data.List
import Data.List1
import Data.SortedMap as M
%default total

-- Return pairwise deltas between elements of a list.
deltas : List1 Integer -> List Integer
deltas (x:::xs) = zipWith (-) xs (x::xs)

-- Index into a map, returning 0 for a missing key.
(!.) : M.SortedMap k Integer -> k -> Integer
(!.) m k = maybe 0 id (M.lookup k m)
infixr 10 !.

-- Given the sorted "init" adapters and the "final" adapters,
-- count possible paths from 0 to final.
countPaths : List1 Integer -> Integer -> Integer
countPaths initAdapters final =
  -- let's do some ~dynamic programming~ with foldr and a map
  let m0 = M.singleton final 1
      f = (\k, m => M.insert k (m!.(k+1) + m!.(k+2) + m!.(k+3)) m)
  in foldr f m0 initAdapters !. 0

main : IO ()
main = do
  ns <- readIntegerLines {a=Integer}
  let sorted = sort ns
  let initAdapters = 0 ::: sorted
  let final = last initAdapters + 3
  let adapters = 0 ::: (sorted ++ [final])
  let ds = deltas adapters
  putStr "*   "; printLn $ count (==1) ds * count (==3) ds
  putStr "**  "; printLn $ countPaths initAdapters final
