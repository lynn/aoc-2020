import Aoc
import Data.List
import Data.List1
import Data.Strings

Password : Type
Password = String

record Policy where
  constructor MkPolicy
  least, most : Nat
  char: Char

||| Parse a line of input like "1-3 a: abcde".
parse : String -> Maybe (Policy, Password)
parse s =
  case split (not . isAlphaNum) s of
    -- ["1", "3", "a", "", "abcde"]
    [sl, sm, sc, "", p] =>
      case (parsePositive sl, parsePositive sm, unpack sc) of
        (Just l, Just m, [c]) => Just (MkPolicy l m c, p)
        _ => Nothing
    _ => Nothing

||| The * password validity test:
conforms1 : Policy -> Password -> Bool
conforms1 policy pw =
  let n = count (== policy.char) (unpack pw)
  in n >= policy.least && n <= policy.most

||| The ** password validity test:
conforms2 : Policy -> Password -> Bool
conforms2 (MkPolicy (S i) (S j) c) pw =
  count ((== Just c) . (`maybeIndex` unpack pw)) [i, j] == 1
conforms2 _ _ = False

main : IO ()
main = do
  ns <- parseLines parse
  putStr "*   "; printLn $ count (uncurry conforms1) ns
  putStr "**  "; printLn $ count (uncurry conforms2) ns
