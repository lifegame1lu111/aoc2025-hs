{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Function ((&))
import Data.List (sortBy)

parse :: T.Text -> ([(Int, Int)], [Int])
parse content = let ~(ranges : ids : _) = T.splitOn "\n\n" content
                    ranges' = ranges
                        & T.lines
                        & map parseRange
                    ids' = ids
                        & T.lines
                        & map (read . T.unpack)
                in (ranges', ids')
  where
    parseRange :: T.Text -> (Int, Int)
    parseRange raw = let ~(a : b : _) = T.splitOn "-" raw
                     in (read . T.unpack $ a, read . T.unpack $ b)

part1 :: ([(Int, Int)], [Int]) -> Int
part1 (ranges, ids) = ids
    & filter go
    & length
  where
    go :: Int -> Bool
    go id = any (\(a, b) -> id >= a && id <= b) ranges

part2 :: [(Int, Int)] -> Int
part2 ranges = go a b sorted
  where
    ~((a, b) : sorted) = sortBy (\(a, _) (a', _) -> compare a a') ranges
    go :: Int -> Int -> [(Int, Int)] -> Int
    go a b [] = b - a + 1
    go a b ((c, d) : rs)
       | c <= b && d > b = go a d rs
       | d <= b = go a b rs
       | otherwise = b - a + 1 + go c d rs

main :: IO ()
main = do
    content <- TIO.readFile "input05.txt"
    
    let parsed = parse content
        result1 = part1 parsed
        result2 = part2 . fst $ parsed

    TIO.putStrLn $ "Part 1: " <> T.show result1
    TIO.putStrLn $ "Part 2: " <> T.show result2

