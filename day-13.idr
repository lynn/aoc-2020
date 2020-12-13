import Aoc
import Data.List
import Data.List1
import Data.Stream
import Data.Strings
%default total

||| Give integers a short name.
Z : Type
Z = Integer

||| A residue class: (k, m) represents all integers ≡ k mod m.
RC : Type
RC = (Z, Z)

||| "Actual" modulo, with (-1) %% 5 == 4
(%%) : Z -> Z -> Z
(%%) a b = ((a `mod` b) + b) `mod` b
infixl 15 %%

||| What's the smallest integer ≥n that's ≡k (mod m)?
nextInRC : RC -> Z -> Z
nextInRC (k, m) n = n + ((k-n) %% m)

||| Given (a,b), compute (g,s,t) so that a*s + b*t = g = gcd(a,b).
partial
egcd : Z -> Z -> (Z, Z, Z)
egcd 0 b = (b, 0, 1)
egcd a b = let (g, s, t) = egcd (b %% a) a in (g, t - s * div b a, s)

||| Try to compute the modular inverse of a (mod m).
inv : Z -> Z -> Maybe Z
inv a m =
  case egcd a m of
    (1, s, _) => Just (s %% m)
    _ => Nothing

||| Try to intersect two RCs into a new RC, with the Chinese remainder theorem.
||| This will only work if m1 and m2 are coprime. But it's ok, because the input is all coprime numbers! Even though this is nowhere specified. So, did I really "solve the puzzle" or overfit and cheat?? merry christmas
crt : RC -> RC -> Maybe RC
crt (k1, m1) (k2, m2) = do
  i <- inv m2 m1
  let k = k2 + m2 * (k1 - k2) * i
  let m = m1 * m2
  pure (k %% m, m)

||| A wait time and bus number for a bus-taking decision.
record TakeBus where
  constructor mkTakeBus
  waitTime : Integer
  bus : Integer

||| Take the first available bus from a list starting at `time`.
partial
takeBusFrom : (buses : List1 Integer) -> (time : Integer) -> TakeBus
takeBusFrom buses time =
  let options = map (\b => mkTakeBus (nextInRC (0, b) time - time) b) buses
  in minimumBy (.waitTime) options

main : IO ()
main = do
  Just time <- parseInteger <$> getLine
    | Nothing => putStrLn "couldn't parse timestamp"
  buses <- map parseInteger . toList . split (==',') <$> getLine
  Just available <- pure $ Data.List1.fromList (catMaybes buses)
    | Nothing => putStrLn "no available buses"

  putStr "*   "
  let result = takeBusFrom available time
  printLn $ result.bus * result.waitTime

  putStr "**  "
  let buses' = mapMaybe (\(i,b) => map (-i,) b) $ enumerate buses
  Just (k, m) <- pure (foldlM crt (0,1) buses')
    | Nothing => putStrLn "bus numbers not coprime"
  printLn k
