{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char as C
import Data.Function
import Data.Functor
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Data.Tuple

import System.Environment
import System.IO.Unsafe (unsafePerformIO)

debug a = seq (unsafePerformIO $ print a)

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
