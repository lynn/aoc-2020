import Aoc
import Data.String.Parser

chain : Parser (a -> a -> a) -> Parser a -> Parser a
chain o t = hchainl t o t

pPlus : Parser (Integer -> Integer -> Integer)
pPlus = (+) <$ string " + "
pTimes : Parser (Integer -> Integer -> Integer)
pTimes = (*) <$ string " * "
pExpr1 : Parser Integer
pExpr1 = chain (pPlus <|> pTimes) $ integer <|> parens pExpr1
pExpr2 : Parser Integer
pExpr2 = chain pTimes $ chain pPlus $ integer <|> parens pExpr2

main : IO ()
main = do
  lines <- readLines
  putStr "*   "; printLn $ sum <$> traverse (map fst . parse pExpr1) lines
  putStr "**  "; printLn $ sum <$> traverse (map fst . parse pExpr2) lines

