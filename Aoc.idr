module Aoc
-- This module defines functions that might be useful every day.

import Data.List
import Data.Strings

%default total

||| Read a list of integers from STDIN, each on its own line.
||| (This is marked "partial", but in practice files are finite.)
public export partial
readIntegerLines : (Num a, Neg a, HasIO io) => io (List a)
readIntegerLines =
  case parseInteger !getLine of
    Just n => (n::) <$> readIntegerLines {a=a}
    Nothing => pure []
