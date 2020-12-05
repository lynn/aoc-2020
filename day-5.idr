import Aoc
import Data.Fin
import Data.List
import Data.Vect
%default total

data FB = F | B
parseFB : Char -> Maybe FB
parseFB 'F' = Just F
parseFB 'B' = Just B
parseFB _ = Nothing

data LR = L | R
parseLR : Char -> Maybe LR
parseLR 'L' = Just L
parseLR 'R' = Just R
parseLR _ = Nothing

data Seat = MkSeat (Vect 7 FB) (Vect 3 LR)
parseSeat : String -> Maybe Seat
parseSeat s =
  case unpack s of
    [a,b,c,d,e,f,g,x,y,z] =>
      MkSeat <$> traverse parseFB [a,b,c,d,e,f,g]
             <*> traverse parseLR [x,y,z]
    _ => Nothing

interface ToBit a where toBit : a -> Fin 2
ToBit FB where toBit F = 0; toBit B = 1
ToBit LR where toBit L = 0; toBit R = 1

fromBits : (ToBit a, Traversable t) => t a -> Nat
fromBits = foldl (\acc, a => 2*acc + finToNat (toBit a)) 0

seatID : Seat -> Nat
seatID (MkSeat row col) = fromBits {a=FB} row * 8 + fromBits {a=LR} col

||| Given a range [a..b] with one number deleted, find that number.
findGap : List Nat -> Maybe Nat
findGap [] = Nothing
findGap [x] = Nothing
findGap (x::y::xs) =
  -- The totality checker wants this "let", but I don't understand why ;v;
  let rest = findGap (y::xs) in if S x == y then rest else Just (S x)

partial
main : IO ()
main = do
  seats <- parseLines parseSeat
  case map seatID seats of
    [] => putStrLn "no seats!"
    ids@(_::_) => do
      -- ^ This @ pattern helps "foldl1" find a proof that "ids" is non-empty.
      putStr "*   "; printLn (foldl1 max ids)
      putStr "**  "; printLn (findGap $ sort ids)
