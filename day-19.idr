import Aoc
import Data.List
import Data.List1
import Data.Strings
import Data.SortedMap
import Debug.Trace

data Production = Terminal String | NTs (List (List Int))
Show Production where
  show (Terminal s) = show s
  show (NTs s) = show s

Rules : Type
Rules = SortedMap Int Production

toRules : List (Int, Production) -> Rules
toRules = fromList

parseRule : String -> Maybe (Int, Production)
parseRule s = do
  (sn:::[rest]) <- pure (split (==':') s) | _ => Nothing
  n <- parsePositive {a=Int} sn
  case split (=='"') rest of
    (" ":::[t,""]) => Just (n, Terminal t)
    _ =>
      let parseChain = traverse parsePositive . forget . split (==' ') . trim
      in case traverse parseChain . forget . split (=='|') . trim $ rest of
        Just nts => Just (n, NTs nts)
        Nothing => Nothing

munch : Eq a => List a -> List a -> Maybe (List a)
munch [] ys = Just ys
munch (x::xs) [] = Nothing
munch (x::xs) (y::ys) = if x == y then munch xs ys else Nothing

mutual
  matches : Rules -> Int -> List Char -> List (List Char)
  matches rs k cs =
    case lookup k rs of
      Nothing => []
      Just (Terminal t) => toList $ munch (unpack t) cs
      Just (NTs alts) => do
        alt <- alts
        matchesSeq rs alt cs

  matchesSeq : Rules -> List Int -> List Char -> List (List Char)
  matchesSeq rs [] cs = [cs]
  matchesSeq rs (k::ks) cs = do
    cs' <- matches rs k cs
    matchesSeq rs ks cs'

matchesFully : Rules -> Int -> String -> Bool
matchesFully rs k s = any (==[]) $ matches rs k (unpack s)

main : IO ()
main = do
  (ls1:::[ls2]) <- readParagraphs | _ => putStrLn "paragraph parse error"
  Just rules <- pure $ toRules <$> traverse parseRule ls1 | _ => putStrLn "rule parse error"
  putStr "*   "; printLn $ count (matchesFully rules 0) ls2
  let rules' = insert 8 (NTs [[42,8],[42]]) $ insert 11 (NTs [[42,31],[42,11,31]]) $ rules
  putStr "**  "; printLn $ count (matchesFully rules' 0) ls2

