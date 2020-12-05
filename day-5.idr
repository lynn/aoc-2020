import Aoc
import Data.Fin
import Data.List
import Data.Vect
%default total

interface ToBit a where
  toBit : a -> Fin 2

fromBinary : (ToBit a, Traversable t) => t a -> Nat
fromBinary = foldl (\acc, a => 2*acc + finToNat (toBit a)) 0

data FB = F | B
parseFB : Char -> Maybe FB
parseFB 'F' = Just F
parseFB 'B' = Just B
parseFB _ = Nothing

ToBit FB where
  toBit F = 0
  toBit B = 1

data LR = L | R
parseLR : Char -> Maybe LR
parseLR 'L' = Just L
parseLR 'R' = Just R
parseLR _ = Nothing

ToBit LR where
  toBit L = 0
  toBit R = 1

Seat : Type
Seat = (Vect 7 FB, Vect 3 LR)

parseSeat : String -> Maybe Seat
parseSeat s =
  case unpack s of
    [a,b,c,d,e,f,g,x,y,z] =>
      case (traverse parseFB $ the (Vect 7 Char) [a,b,c,d,e,f,g],
            traverse parseLR $ the (Vect 3 Char) [x,y,z]) of
        (Just row, Just col) => Just (row, col)
        _ => Nothing
    _ => Nothing

seatID : Seat -> Nat
seatID (row, col) =
  let r = fromBinary {a=FB} row
      c = fromBinary {a=LR} col
  in 8 * r + c

||| Given a range [a..b] with one number deleted, find that number.
findGap : List Nat -> Maybe Nat
findGap [] = Nothing
findGap [x] = Nothing
findGap (x::y::xs) =
  -- The totality checker wants this, but I don't understand why ;v;
  let rest = findGap (y::xs) in
    if S x == y then rest else Just (S x)

partial
main : IO ()
main = do
  seats <- parseLines parseSeat
  let ids = map seatID seats
  case ids of
    [] => putStrLn "no seats!"
    ids@(_::_) => do
      -- ^ This @ pattern helps "foldl1" find a proof that "ids" is non-empty.
      let maxID = foldl1 max ids
      let minID = foldl1 min ids
      putStr "*   "; printLn maxID
      putStr "**  "; printLn (findGap $ sort ids)
