import Aoc
import Data.List
import Data.List1
%default total

partial
main : IO ()
main = do
  ps <- readParagraphs
  putStr "*   "; printLn $ sum $ map (maybe 0 length . foldl1' union     . map unpack) ps
  putStr "**  "; printLn $ sum $ map (maybe 0 length . foldl1' intersect . map unpack) ps
