module Aoc
-- This module defines functions that might be useful every day.

import Data.List
import Data.Strings

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

