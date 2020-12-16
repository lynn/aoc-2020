import Aoc
import Data.List
import Data.List1
import Data.Strings
%default total

record Range where
  constructor mkRange
  low : Int
  high : Int
Show Range where
  show r = show r.low ++ "~" ++ show r.high

inRange : Range -> Int -> Bool
inRange r i = i >= r.low && i <= r.high

inRanges : List Range -> Int -> Bool
inRanges rs i = any (flip inRange i) rs

record Field where
  constructor mkField
  name : String
  ranges : List Range

Show Field where
  show f = show (f.name, f.ranges)

parseRange : String -> Maybe Range
parseRange s =
  case split (=='-') s of
    (a:::[b]) => mkRange <$> parseInteger a <*> parseInteger b
    _ => Nothing

parseField : String -> Maybe Field
parseField s =
  case split (==':') s of
    (name:::[rs]) => Just $ mkField name $ mapMaybe parseRange $ assert_total words rs
    _ => Nothing

onlyOne : List a -> Maybe a
onlyOne [x] = Just x
onlyOne _ = Nothing

whittle : Eq a => List (List a) -> List (List a)
whittle sets =
  let isolated = mapMaybe onlyOne sets
  in map (\s => if length s == 1 then s else filter (not . (`elem` isolated)) s) sets

main : IO ()
main = do
  let pcsi = parseCommaSeparatedIntegers {a=Int}
  (sfs:::[[_,smt],_::snts]) <- readParagraphs | _ => putStrLn "need 3 paragraphs"
  Just fs <- pure (traverse parseField sfs) | _ => putStrLn "fields parse error"
  Just mt <- pure (pcsi smt) | _ => putStrLn "my ticket parse error"
  Just nts <- pure (traverse pcsi snts) | _ => putStrLn "nearby tickets parse error"

  let values = concat nts
  let validAtAll = \i => any (`inRange` i) (concatMap (.ranges) fs)
  putStr "*   "; printLn $ sum $ filter (not . validAtAll) values

  let validTickets = filter (all validAtAll) nts
  let columns = transpose validTickets
  let sudoku = [[f.name | f <- fs, all (inRanges f.ranges) col] | col <- columns]
  let solution = times 40 whittle sudoku
  Just fieldOrder <- pure $ traverse onlyOne solution | _ => putStrLn "not enough progress"
  putStr "**  "; printLn $ product [v | (k,v) <- zip fieldOrder mt, "departure" `isPrefixOf` k]

