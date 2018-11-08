module Day1.Day1 where

day1 :: FilePath -> IO Integer
day1 path = do
  str <- readFile path
  return $ foldr (+) 0 $
    map (\c -> case c of
      '(' -> 1
      ')' -> -1
      _ -> error $ "unexpected char " ++ show c) str

day1' :: FilePath -> IO Integer
day1' path = do
  str <- readFile path
  return $ findPos 0 1 str

findPos :: Integer -> Integer -> String -> Integer
findPos curFloor curPos remaining =
  let floorDelta = case remaining of
                     '(' : _ -> 1
                     ')' : _ -> -1
                     c : _ -> error $ "unexpected char" ++ show c
                     _ -> error $ "no more floors"
      curFloor' = curFloor + floorDelta
  in
    if curFloor' == -1 then
      curPos
    else
      findPos curFloor' (curPos + 1) $ tail remaining
