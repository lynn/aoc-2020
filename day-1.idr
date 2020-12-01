import Aoc
import Data.Vect

||| Get all ways to choose n distinct elements from a list.
combinations : (n : Nat) -> List a -> List (Vect n a)
combinations Z _ = [[]]
combinations (S k) [] = []
combinations (S k) (x::xs) =
  [x::c | c <- combinations k xs] ++ combinations (S k) xs

main : IO ()
main = do
  ns <- readIntegerLines {a=Int}
  putStr "*   "; printLn [product c | c <- combinations 2 ns, sum c == 2020]
  putStr "**  "; printLn [product c | c <- combinations 3 ns, sum c == 2020]
