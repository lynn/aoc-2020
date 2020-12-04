import Aoc
import Data.Fin
import Data.List
import Data.List1
import Data.Nat
import Data.Strings
import Data.Vect
%default total

Passport : Type
Passport = List (String, String)

||| Split a string at the first colon in it: "abc:def" -> ("abc", "def").
splitColon : String -> (String, String)
splitColon s = let (p, q) = break (==':') s in (p, pack $ drop 1 $ unpack q)

requiredFields : List String
requiredFields = ["byr","iyr","eyr","hgt","hcl","ecl","pid"] -- not "cid"

||| *: Are all required fields present?
isValid1 : Passport -> Bool
isValid1 passport =
  all (\k => any ((==k) . fst) passport) requiredFields

||| Check if a string represents a natural number in a given range [lo, hi].
isNatInRange : Nat -> Nat -> String -> Bool
isNatInRange lo hi s =
  case parsePositive s of
    Nothing => False
    Just n => n >= lo && n <= hi

||| Validate a passport field.
isValidField : (String, String) -> Bool
isValidField (k, v) =
  case k of
    "byr" => isNatInRange 1920 2002 v
    "iyr" => isNatInRange 2010 2020 v
    "eyr" => isNatInRange 2020 2030 v
    "hgt" =>
      case span isDigit v of
        (n, "cm") => isNatInRange 150 193 n
        (n, "in") => isNatInRange 59 76 n
        _ => False
    "hcl" =>
      case unpack v of
        ('#'::xs) => length xs == 6 && all isLowerHexDigit xs
        _ => False
    "ecl" => Prelude.elem v ["amb","blu","brn","gry","grn","hzl","oth"]
    "pid" => let xs = unpack v in length xs == 9 && all isDigit xs
    "cid" => True
    _ => False

||| **: Are all required field present, and are all fields valid?
isValid2 : Passport -> Bool
isValid2 passport =
  isValid1 passport && all isValidField passport

partial
main : IO ()
main = do
  paras <- readParagraphs
  let passports = map (map splitColon . words . unwords) paras
  putStr "*   "; printLn (count isValid1 passports)
  putStr "**  "; printLn (count isValid2 passports)
