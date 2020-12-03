module Aoc
-- This module defines functions that might be useful every day.

import Data.Nat
import Data.List
import Data.Stream
import Data.Strings
import Data.Vect

%default total

||| Apply a parser to each line of STDIN until it fails.
||| (`getLine` eventually returns an empty string, which
||| the parser should return Nothing for.)
public export partial
parseLines : HasIO io => (String -> Maybe a) -> io (List a)
parseLines parser =
  case parser !getLine of
    Just n => (n::) <$> parseLines parser
    Nothing => pure []

||| Read a list of non-empty strings from STDIN.
public export partial
readLines : HasIO io => io (List String)
readLines = parseLines (\s => if s == "" then Nothing else Just s)

||| Read a list of integers from STDIN, each on its own line.
public export partial
readIntegerLines : (Num a, Neg a, HasIO io) => io (List a)
readIntegerLines = parseLines parseInteger

||| Count how many times a predicate is true in a list.
public export
count : (a -> Bool) -> List a -> Nat
count f = length . filter f

||| Try to index into a list.
public export
maybeIndex : Nat -> List a -> Maybe a
maybeIndex n xs =
  case inBounds n xs of
    Yes _ => Just (index n xs)
    _ => Nothing

||| Turn a list into a vector of length n, if possible.
||| (This is in the stdlib in newer versions of Idris2 than mine.)
public export
toVect : (n : Nat) -> List a -> Maybe (Vect n a)
toVect Z [] = Just []
toVect (S k) (x :: xs) = (x::) <$> toVect k xs
toVect _ _ = Nothing

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
