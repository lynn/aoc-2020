import Aoc
import Data.List
import Data.List1
import Data.Strings

Password : Type
Password = String

record Policy where
  constructor MkPolicy
  low, high : Nat
  char : Char

||| Parse a line of input like "1-3 a: abcde".
parse : String -> Maybe (Policy, Password)
parse s =
  case split (not . isAlphaNum) s of
    -- ["1", "3", "a", "", "abcde"]
    [sl, sh, sc, "", p] =>
      case (parsePositive sl, parsePositive sh, unpack sc) of
        (Just l, Just h, [c]) => Just (MkPolicy l h c, p)
        _ => Nothing
    _ => Nothing

||| The * password validity test:
conforms1 : Policy -> Password -> Bool
conforms1 policy pw =
  let n = count (== policy.char) (unpack pw)
  in n >= policy.low && n <= policy.high

||| The ** password validity test:
conforms2 : Policy -> Password -> Bool
conforms2 (MkPolicy (S i) (S j) c) pw =
  count {t=List} ((== Just c) . (`maybeIndex` unpack pw)) [i, j] == 1
conforms2 _ _ = False

main : IO ()
main = do
  pairs <- parseLines parse
  putStr "*   "; printLn $ count (uncurry conforms1) pairs
  putStr "**  "; printLn $ count (uncurry conforms2) pairs
