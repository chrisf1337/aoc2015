module Day2.Day2 where

import Text.Parsec
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char

parseLine :: Parsec String () (Integer, Integer, Integer)
parseLine = do
  x <- read <$> many1 digit
  char 'x'
  y <- read <$> many1 digit
  char 'x'
  z <- read <$> many1 digit
  return $ (x, y, z)

parseAll :: Parsec String () [(Integer, Integer, Integer)]
parseAll = sepEndBy parseLine spaces

area :: (Integer, Integer, Integer) -> Integer
area (l, w, h) =
  let a = l * w
      b = l * h
      c = w * h
  in
    2 * (a + b + c) + minimum [a, b, c]

smallestPerim :: (Integer, Integer, Integer) -> Integer
smallestPerim (l, w, h) =
  let a = 2 * (l + w)
      b = 2 * (l + h)
      c = 2 * (w + h)
  in
    minimum [a, b, c]

day2 :: FilePath -> IO Integer
day2 path = do
  str <- readFile path
  case parse parseAll "" str of
    Left err -> do
      putStrLn $ show err
      return 0
    Right dims -> return $ foldr (+) 0 $ map area dims

day2' :: FilePath -> IO Integer
day2' path = do
  str <- readFile path
  case parse parseAll "" str of
    Left err -> do
      putStrLn $ show err
      return 0
    Right dims -> return $ foldr (+) 0 $ map (\(l, w, h) -> smallestPerim (l, w, h) + l * w * h) dims
