import Aoc
import Data.Fin
import Data.List
import Data.Nat
import Data.Stream
import Data.Vect
%default total

data Square = Tree | Open

isTree : Square -> Bool
isTree Tree = True
isTree _ = False

||| Parse a character of input as a grid square.
parseSquare : Char -> Maybe Square
parseSquare '#' = Just Tree
parseSquare '.' = Just Open
parseSquare _ = Nothing

||| A grid of n rows, each having (m'+1) squares.
Grid : (n : Nat) -> (m' : Nat) -> Type
Grid n m' = Vect n (Vect (S m') Square)

data ParseError = UnrecognizedChar | NotRectangular | WidthZero
Show ParseError where
  show UnrecognizedChar = "unrecognized char"
  show NotRectangular = "not rectangular"
  show WidthZero = "width is zero"

||| Parse the input grid (given as a list of lines).
parseGrid : List String -> Either ParseError (n : Nat ** m' : Nat ** Grid n m')
parseGrid ls =
  case traverse (traverse parseSquare . unpack) ls of
    Nothing => Left UnrecognizedChar
    Just rs =>
      let n = length ls
          m = case ls of l::_ => length l; _ => Z
      in case toMatrix n m rs of
        Nothing => Left NotRectangular
        Just mat =>
          case m of
            Z => Left WidthZero
            S m' => Right (n ** (m' ** mat))

||| Get elements 0, k+1, 2(k+1), ... of a list.
||| (That is: yield an element, skip k elements, repeat.)
gaps : Nat -> List a -> List a
gaps k xs = go Z xs
  where go : Nat -> List a -> List a
        go _ [] = []
        go Z (x::xs) = x :: go k xs
        go (S i) (x::xs) = go i xs

||| Get all squares we run into on the slope (right, down) in the grid.
||| "down" must be provably positive: otherwise we'd never reach the bottom.
traceSlope : {m' : Nat} -> (right : Nat) -> (down : Nat) -> {auto _ : IsSucc down} -> Grid n m' -> List Square
traceSlope right down@(S d') grid =
  let rows = gaps d' (toList grid)
  in drop 1 $ go Z rows
  where go : Nat -> List (Vect (S m') Square) -> List Square
        go i (r::rs) = mindex i r :: go (i + right) rs
        go i [] = []

||| Get the "score" (number of trees) for a slope.
score : {m' : Nat} -> (right : Nat) -> (down : Nat) -> {auto _ : IsSucc down} -> Grid n m' -> Nat
score right down grid =
  count isTree (traceSlope right down grid)

partial
main : IO ()
main = do
  ls <- readLines
  case parseGrid ls of
    Left error => printLn error
    Right (n ** (m' ** grid)) => do
      putStr "*   "
      printLn $ score 3 1 grid
      putStr "**  "
      let slopes : List (Nat, Nat) = [(1,1),(3,1),(5,1),(7,1),(1,2)]
      printLn $ product [score r d grid | (r,d@(S _)) <- slopes]
