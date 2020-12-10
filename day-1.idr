import Aoc
import Data.Vect
%default total

main : IO ()
main = do
  ns <- readIntegerLines {a=Int}
  putStr "*   "; printLn [product c | c <- combinations 2 ns, sum c == 2020]
  putStr "**  "; printLn [product c | c <- combinations 3 ns, sum c == 2020]
