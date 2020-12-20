import Aoc
import Data.List
import Data.Vect
import Data.SortedSet as S

Point : Nat -> Type
Point n = Vect n Int

replicateM : Monad m => (n : Nat) -> m a -> m (Vect n a)
replicateM Z _ = pure []
replicateM (S k) m = (::) <$> m <*> replicateM k m

neighbors : {n : Nat} -> Point n -> List (Point n)
neighbors v =
  [zipWith (+) v d | d <- replicateM n [-1..1], any (/=0) d]

step : {n : Nat} -> SortedSet (Point n) -> SortedSet (Point n)
step ps =
  let ps' = S.toList ps
  in S.fromList [p | p <- nub $ ps' ++ concatMap neighbors ps',
                 let k = count (flip S.contains ps) (neighbors p),
                 k == 3 || k == 2 && S.contains p ps]

main : IO ()
main = do
  ls <- readLines
  let initial = [(x,y) | (y,line) <- enumerate ls, (x,c) <- enumerate (unpack line), c == '#']
  let initial3 = S.fromList [[the Int $ cast x,cast y,0] | (x,y) <- initial]
  let initial4 = S.fromList [[the Int $ cast x,cast y,0,0] | (x,y) <- initial]
  putStr "*   "; printLn $ length $ S.toList $ times 6 step initial3
  putStr "**  "; printLn $ length $ S.toList $ times 6 step initial4
