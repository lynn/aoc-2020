import Aoc
import Data.List
import Data.Nat
import Data.Strings
import Data.SortedSet as S
%default total
-- TODO: this broke when I upgraded from 0.2.1 to 0.2.1-0a685f8d2... merry christmas

data Instruction
  = Nop Integer
  | Acc Integer
  | Jmp Integer

Program : Type
Program = List Instruction

parseInstruction : String -> Maybe Instruction
parseInstruction s =
  case words s of
    -- thankfully parseInteger has no problem with "+3"
    ["nop", n] => Nop <$> parseInteger n
    ["acc", n] => Acc <$> parseInteger n
    ["jmp", n] => Jmp <$> parseInteger n
    _ => Nothing

data Outcome = InfiniteLoop Integer | Terminated Integer
Show Outcome where
  show (InfiniteLoop acc) = "acc = " ++ show acc ++ " before infinite loop"
  show (Terminated acc) = "terminated with acc = " ++ show acc

-- This one's partial because... you know... (gestures)
partial
runFrom : (pc : Nat) -> (acc : Integer) -> (visited : SortedSet Nat) -> Program -> Outcome
runFrom pc acc visited program =
  if S.contains pc visited then InfiniteLoop acc else
    let visited' = S.insert pc visited in
    case maybeIndex pc program of
      Just (Nop _) => runFrom (pc+1) acc visited' program
      Just (Acc n) => runFrom (pc+1) (acc+n) visited' program
      Just (Jmp n) => runFrom (pc+integerToNat n) acc visited' program
      Nothing      => Terminated acc

partial
run : Program -> Outcome
run program = runFrom 0 0 S.empty program

||| Given an "edit" function that maybe applies to one element of the list,
||| try to apply it at every position in the list.
||| e.g.  halve n = if mod n 2 == 0 then Just (div n 2) else Nothing
|||       tryEach halve [10..13] = [[5,11,12,13], [10,11,6,13]]
tryEach : (a -> Maybe a) -> List a -> List (List a)
tryEach f xs = go f xs [] where
  go : (a -> Maybe a) -> List a -> List a -> List (List a)
  go f [] _ = []
  go f (x::xs) pre =
    case f x of
      Just y  => reverseOnto (y::xs) pre :: go f xs (x::pre)
      Nothing =>                            go f xs (x::pre)

||| Change nops into jmps and vice versa.
edit : Instruction -> Maybe Instruction
edit (Nop n) = Just $ Jmp n
edit (Jmp n) = Just $ Nop n
edit _ = Nothing

||| Extract Just a terminated accumulator from an outcome, else Nothing.
justTerminated : Outcome -> Maybe Integer
justTerminated (Terminated acc) = Just acc
justTerminated _ = Nothing

partial
main : IO ()
main = do
  program <- parseLines parseInstruction
  printLn $ length program
  putStr "*   "
  printLn $ run program
  putStr "**  "
  printLn $ mapMaybe (justTerminated . run) (tryEach edit program)
