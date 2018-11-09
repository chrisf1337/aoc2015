module Day3.Day3 where

import Control.Monad.Trans.State.Lazy
import Data.HashMap.Strict

day3 :: FilePath -> IO Integer
day3 path = do
  str <- readFile path
  return 0

move :: Char -> State ((Integer, Integer), HashMap String Integer) ()
move = undefined
-- move dir = do
--   (pos, visited) <- get
--   case dir of
--     '>' ->
