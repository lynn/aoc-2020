import Aoc
import Data.Strings
import Data.SortedMap

band : Integer -> Integer -> Integer
band = prim__and_Integer

bor : Integer -> Integer -> Integer
bor = prim__or_Integer

bxor : Integer -> Integer -> Integer
bxor i j = cast $ prim__xor_Int (cast i) (cast j)

||| surrounded "[" "]" "[abcd]" == Just "abcd"
||| surrounded "[" "]" "[abc" == Nothing
surrounded : String -> String -> String -> Maybe String
surrounded l r s =
  let le = cast . length
  in if (l `isPrefixOf` s) && (r `isSuffixOf` s)
    then Just $ strSubstr (le l) (le s - le l - le r) s
    else Nothing

record Mask where
  constructor mkMask
  nonZeros : Integer
  ones : Integer
  floatXors : List Integer

mask0 : Mask
mask0 = mkMask 0 0 [0]

applyMask : Mask -> Integer -> Integer
applyMask m = bor m.ones . band m.nonZeros

applyMaskIndex : Mask -> Integer -> List Integer
applyMaskIndex m i = map (bxor (bor m.ones i)) m.floatXors

record State where
  constructor mkState
  mask : Mask
  memory : SortedMap Integer Integer

state0 : State
state0 = mkState mask0 empty

data Instruction = SetMask Mask | SetMem Integer Integer

parseMask : String -> Maybe Mask
parseMask = map (foldl go mask0) . traverse parseChar . unpack
  where
    data MaskChar = X | Zero | One
    parseChar : Char -> Maybe MaskChar
    parseChar 'X' = Just X
    parseChar '0' = Just Zero
    parseChar '1' = Just One
    parseChar _ = Nothing
    go : Mask -> MaskChar -> Mask
    go (mkMask nz o fx) X    = mkMask (2*nz+1) (2*o)   [2*x+i | x<-fx, i<-[0,1]]
    go (mkMask nz o fx) Zero = mkMask (2*nz)   (2*o)   (map (2*) fx)
    go (mkMask nz o fx) One  = mkMask (2*nz+1) (2*o+1) (map (2*) fx)

parseInstruction : String -> Maybe Instruction
parseInstruction s =
  case words s of
    ["mask","=",v] => SetMask <$> parseMask v
    [i,"=",v] => SetMem <$> (surrounded "mem[" "]" i >>= parseInteger) <*> parseInteger v
    _ => Nothing

runInstruction1 : State -> Instruction -> State
runInstruction1 s (SetMask m) = record { mask = m } s
runInstruction1 s (SetMem i v) =
  record { memory $= insert i (applyMask s.mask v) } s

runInstruction2 : State -> Instruction -> State
runInstruction2 s (SetMask m) = record { mask = m } s
runInstruction2 s (SetMem i v) =
  let js = applyMaskIndex s.mask i
      memory' = foldl (\m, j => insert j v m) s.memory js
  in record { memory = memory' } s

main : IO ()
main = do
  instructions <- parseLines parseInstruction
  let sumWith = (\f => sum $ .memory $ foldl f state0 instructions)
  putStr "*   "; printLn (sumWith runInstruction1)
  putStr "**  "; printLn (sumWith runInstruction2)
