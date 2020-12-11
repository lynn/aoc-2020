import Aoc
import Data.List
import Data.SortedSet as S
%default total
-- this is slow

Point : Type
Point = (Integer, Integer)

||| Add two points.
(+~) : Point -> Point -> Point
(+~) (x,y) (x',y') = (x+x',y+y')
infixl 5 +~

Points : Type
Points = S.SortedSet Point

||| Filter a set.
filter : Ord k => (k -> Bool) -> SortedSet k -> SortedSet k
filter f = S.fromList . Data.List.filter f . S.toList

||| Apply "f" until the result stops changing, as judged by "eq".
partial
eqFixBy : (eq : a -> a -> Bool) -> (f : a -> a) -> (x : a) -> a
eqFixBy eq f x = let x' = f x in if eq x x' then x else eqFixBy eq f x'

||| Are two sets equal?
setEq : Ord k => S.SortedSet k -> S.SortedSet k -> Bool
setEq s1 s2 = S.toList s1 == S.toList s2

||| Apply a step function to the empty set until the result stops changing.
||| Then measure the size of the set.
partial
stableSize : (Points -> Points) -> Nat
stableSize f = length . S.toList $ eqFixBy setEq f S.empty

||| The 8 directions around (0,0).
directions : List Point
directions = [(-1,-1),( 0,-1),( 1,-1),
              (-1, 0),        ( 1, 0),
              (-1, 1),( 0, 1),( 1, 1)]

||| All immediate neighbor coordinates of a point.
neighbors1 : Point -> Points
neighbors1 p = S.fromList $ map (p+~) directions

||| Step to the next "occupied seats set" with a cellular automaton rule.
step : (neighbors : Point -> Points) -> (threshold : Nat) -> (seats : Points) -> (occupied : Points) -> Points
step neighbors threshold seats occupied = filter nowOccupied seats
  where
    nowOccupied : Point -> Bool
    nowOccupied p =
      let occ = (`S.contains` occupied)
          c = count occ (neighbors p)
      in if occ p then c < threshold else c == 0

||| Look for a point in a set in a given direction.
||| Give up when we're out of fuel.
seek : (fuel : Nat) -> Point -> Points -> Point -> Maybe Point
seek Z _ _ _ = Nothing
seek (S k) p ps dir =
  let p' = p +~ dir in
    if S.contains p' ps then Just p' else seek k p' ps dir

||| Seek for seats in all 8 directions.
||| (The fuel value "20" is eyeballed from the input ☠️)
neighbors2 : (seats : Points) -> Point -> Points
neighbors2 seats p = S.fromList $ mapMaybe (seek 20 p seats) directions

partial
main : IO ()
main = do
  ls <- readLines
  let seats = S.fromList [(x,y) | (y,line) <- enumerate ls, (x,c) <- enumerate (unpack line), c == 'L']
  putStr "*   "; printLn $ stableSize (step neighbors1 4 seats)
  putStr "**  "; printLn $ stableSize (step (neighbors2 seats) 5 seats)
