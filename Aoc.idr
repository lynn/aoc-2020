module Aoc
-- This module defines functions that might be useful every day.

import Data.Nat
import Data.List
import Data.List1
import Data.Stream
import Data.Strings
import Data.Vect

%default total

||| Apply a parser to each line of STDIN until it fails.
||| (`getLine` eventually returns an empty string, which
||| the parser should return Nothing for.)
|||
||| We use assert_total, because Idris doesn't know that our input files are finite.
public export
parseLines : HasIO io => (String -> Maybe a) -> io (List a)
parseLines parser =
  case parser !getLine of
    Just n => (n::) <$> assert_total (parseLines parser)
    Nothing => pure []

||| Read a list of non-empty strings from STDIN.
public export
readLines : HasIO io => io (List String)
readLines = parseLines (\s => if s == "" then Nothing else Just s)

||| Read a list of paragraphs from STDIN.
public export
readParagraphs : HasIO io => io (List1 (List String))
readParagraphs = split (== "") <$> go
  where go : io (List String); go = assert_total $ do
    l <- getLine
    case l of
      "" => do
        m <- getLine
        case m of
          "" => pure []
          _ => (l::) . (m::) <$> go
      _ => (l::) <$> go

||| Read a list of integers from STDIN, each on its own line.
public export
readIntegerLines : (Num a, Neg a, HasIO io) => io (List a)
readIntegerLines = parseLines parseInteger

||| Count how many times a predicate is true.
public export
count : Foldable t => (a -> Bool) -> t a -> Nat
count f = foldr (\x, acc => if f x then S acc else acc) 0

||| Try to index into a list.
public export
maybeIndex : Nat -> List a -> Maybe a
maybeIndex n xs =
  case inBounds n xs of
    Yes _ => Just (index n xs)
    _ => Nothing

||| Turn a list of lists into an (n x m) matrix, if possible.
public export
toMatrix : (n : Nat) -> (m : Nat) -> List (List a) -> Maybe (Vect n (Vect m a))
toMatrix n m rows =
    sequence (map (toVect m) rows) >>= toVect n

||| Modularly index a non-empty vector.
public export
mindex : {n : Nat} -> (i : Nat) -> Vect (S n) a -> a
mindex i (x::xs) =
  let i' = modNatNZ i (S n) SIsNotZ  -- really just an optimization
  in index i' (cycle (x::toList xs))

public export
isLowerHexDigit : Char -> Bool
isLowerHexDigit c = isDigit c || c >= 'a' && c <= 'f'

public export
takeVect : (n : Nat) -> List a -> Maybe (Vect n a, List a)
takeVect Z xs = Just ([], xs)
takeVect (S n) [] = Nothing
takeVect (S n) (x::xs) = do { (v, l) <- takeVect n xs; pure (x::v, l) }

||| Get all ways to choose n distinct elements from a list.
public export
combinations : (n : Nat) -> List a -> List (Vect n a)
combinations Z _ = [[]]
combinations (S k) [] = []
combinations (S k) (x::xs) =
  [x::c | c <- combinations k xs] ++ combinations (S k) xs

public export
scanl : (f : a -> b -> a) -> a -> List b -> List a
scanl f a [] = [a]
scanl f a (b::bs) = a :: scanl f (f a b) bs

public export
scanl1 : (f : a -> a -> a) -> List a -> List a
scanl1 f [] = []
scanl1 f (x::xs) = scanl f x xs
