{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.Bifunctor (first, second)
import Data.Char as C
import Data.Function
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord

import System.Environment

solve1 :: [Int] -> Int
solve1 = L.foldl' (+) 0

solve2 :: [Int] -> Int
solve2 = L.foldl' (*) 1

solutions =
  [ solve1
  , solve2
  ]

numbers =
  map (read @Int)
    . filter (C.isNumber . head)
    . L.groupBy ((==) `on` C.isNumber)

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . fmap (read @Int)
    . lines
    =<< getContents
