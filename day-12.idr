import Aoc
import Data.Nat
import Data.Strings

Point : Type
Point = (Integer, Integer)

||| Add two points.
(+~) : Point -> Point -> Point
(+~) (x,y) (x',y') = (x+x',y+y')
infixl 5 +~

||| Scale a point.
(*~) : Integer -> Point -> Point
(*~) n (x,y) = (n*x,n*y)
infixl 6 *~

||| Turn a vector 90° to the left. (+x is >, +y is ^)
turnLeft : Point -> Point
turnLeft (x,y) = (-y,x)

||| Turn a vector 90° to the right. (+x is >, +y is ^)
turnRight : Point -> Point
turnRight (x,y) = (y,-x)

||| Apply a function n times.
times : Nat -> (a -> a) -> a -> a
times Z f x = x
times (S k) f x = times k f (f x)

manhattanNorm : Point -> Integer
manhattanNorm (x,y) = abs x + abs y

record Ship where
  constructor MkShip
  position : Point
  direction : Point

data Instruction
  = North Integer
  | South Integer
  | East Integer
  | West Integer
  | Left90 Nat
  | Right90 Nat
  | Forward Integer

||| Move a ship's position.
move : Point -> Ship -> Ship
move d s = record { position $= (+~ d) } s

||| Move a ship's waypoint (direction vector).
moveWaypoint : Point -> Ship -> Ship
moveWaypoint d s = record { direction $= (+~ d) } s

||| Follow an instruction, * style.
step1 : Ship -> Instruction -> Ship
step1 s (North n) = move (0, n) s
step1 s (South n) = move (0, -n) s
step1 s (East n) = move (n, 0) s
step1 s (West n) = move (-n, 0) s
step1 s (Left90 n) = record { direction $= times n turnLeft } s
step1 s (Right90 n) = record { direction $= times n turnRight } s
step1 s (Forward n) = move (n *~ s.direction) s

||| Follow an instruction, ** style.
step2 : Ship -> Instruction -> Ship
step2 s (North n) = moveWaypoint (0, n) s
step2 s (South n) = moveWaypoint (0, -n) s
step2 s (East n) = moveWaypoint (n, 0) s
step2 s (West n) = moveWaypoint (-n, 0) s
step2 s (Left90 n) = record { direction $= times n turnLeft } s
step2 s (Right90 n) = record { direction $= times n turnRight } s
step2 s (Forward n) = move (n *~ s.direction) s

||| Try to divide a natural number by 90.
div90 : Nat -> Maybe Nat
div90 n =
  case divmodNatNZ n 90 SIsNotZ of
    (k, 0) => Just k
    _ => Nothing

||| Try to parse an instruciton.
parseInstruction : String -> Maybe Instruction
parseInstruction s =
  case break isDigit s of
    ("N", n) => North <$> parsePositive n
    ("S", n) => South <$> parsePositive n
    ("E", n) => East <$> parsePositive n
    ("W", n) => West <$> parsePositive n
    ("L", n) => Left90 <$> (parsePositive n >>= div90)
    ("R", n) => Right90 <$> (parsePositive n >>= div90)
    ("F", n) => Forward <$> parsePositive n
    _ => Nothing

main : IO ()
main = do
  xs <- parseLines parseInstruction
  let ship1 = foldl step1 (MkShip (0, 0) (1, 0)) xs
  putStr "*   "; printLn (manhattanNorm ship1.position)
  let ship2 = foldl step2 (MkShip (0, 0) (10, 1)) xs
  putStr "**  "; printLn (manhattanNorm ship2.position)
