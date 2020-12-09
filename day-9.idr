import Aoc
import Data.List
import Data.Nat
import Data.Strings
import Data.Vect
%default total

||| Shift an element onto the end of a vector, deleting the first.
shiftVec : a -> Vect (S n) a -> Vect (S n) a
shiftVec x (_::t) = rewrite plusCommutative 1 n in t ++ [x]

||| Find an "invalid" number in the list, using a window, which is initially the preamble.
findInvalid : {n : Nat} -> Vect (S n) Int -> List Int -> Maybe Int
findInvalid window [] = Nothing
findInvalid window (x::xs) =
  if any ((==x) . sum) $ combinations 2 $ toList window
    then findInvalid (shiftVec x window) xs
    else Just x

||| slice 3 5 xs == [xs!!3, xs!!4, xs!!5]
slice : Nat -> Nat -> List a -> List a
slice lo hi = take (minus (S hi) lo) . drop lo

||| Given n and xs, return all (i,j) so that (i<j) and sum (slice i j xs) == n.
rangesSummingTo : (Eq a, Num a, Neg a) => a -> List a -> List (Nat, Nat)
rangesSummingTo n xs =
  let ys = scanl1 (+) xs
      cs = combinations 2 (zip [0..length ys] ys)
  in [(i+1, j) | [(i,y1),(j,y2)] <- cs, y2 - y1 == n, i+1 < j]

main : IO ()
main = do
  xs <- readIntegerLines {a=Int}
  putStr "*   "
  Just (preamble, rest) <- pure (takeVect 25 xs) | Nothing => putStrLn "input too short!"
  Just n <- pure (findInvalid preamble rest)     | Nothing => putStrLn "found no answer to *"
  printLn n
  putStr "**  "
  [(lo, hi)] <- pure (rangesSummingTo n xs) | [] => putStrLn "no solution for **"
                                            | _  => putStrLn "multiple solutions for **"
  s@(_::_) <- pure (slice lo hi xs)         | [] => putStrLn "solution for ** is empty"
  printLn (foldl1 min s + foldl1 max s)
