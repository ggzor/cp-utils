{-# LANGUAGE TypeApplications #-}

import qualified Data.List as L
import System.Environment

solve1 :: [Int] -> Int
solve1 = L.foldl' (+) 0

solve2 :: [Int] -> Int
solve2 = L.foldl' (*) 1

solutions =
  [ solve1
  , solve2
  ]

main :: IO ()
main = do
  idx <- read . head <$> getArgs
  print
    . (solutions !! (idx - 1))
    . fmap (read @Int)
    . lines
    =<< getContents
