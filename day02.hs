{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

discreteLog10 :: Integer -> Integer
discreteLog10 = \case
    n | n < 10 -> 0
    n -> 1 + discreteLog10 (n `quot` 10)

parse :: T.Text -> [[Integer]]
parse = go . T.splitOn ","
  where
    go :: [T.Text] -> [[Integer]]
    go = \case
        [] -> []
        r : rs -> let ~(lo : hi : _) = read . T.unpack <$> T.splitOn "-" r
                      szLo = discreteLog10 lo + 1
                      szHi = discreteLog10 hi + 1
                  in [lo..hi] : go rs

part1 :: [[Integer]] -> Integer
part1 = sum . map go
  where
    go :: [Integer] -> Integer
    go = sum . filter go'

    go' :: Integer -> Bool
    go' n = let sz = discreteLog10 n + 1
                mask = 10 ^ (sz `quot` 2)
            in even sz && n `quot` mask == n `rem` mask

part2 :: [[Integer]] -> Integer
part2 = sum . map go
  where
    go :: [Integer] -> Integer
    go = sum . filter go'

    go' :: Integer -> Bool
    go' n = let sz = discreteLog10 n + 1
                ds = lookupTable !! fromInteger sz
            in sz >= 2 && any (verifyID n sz) ds

    verifyID :: Integer -> Integer -> Integer -> Bool
    verifyID n sz d = let mask = 10 ^ d
                          steps = iterate (`quot` mask) n
                          chunks = (`rem` mask) <$> take (fromInteger $ sz `quot` d) steps
                      in all (== head chunks) chunks

    lookupTable :: [[Integer]]
    lookupTable = [ [ 0 ]
                  , [ 1 ]
                  , [ 1 ]
                  , [ 1 ]
                  , [ 1, 2 ]
                  , [ 1 ]
                  , [ 1, 2, 3 ]
                  , [ 1 ]
                  , [ 1, 2, 4 ]
                  , [ 1, 3 ]
                  , [ 1, 2, 5 ]
                  ]


main :: IO ()
main = do
    content <- TIO.readFile "input02.txt"

    let parsed = parse content
        result1 = part1 parsed
        result2 = part2 parsed

    TIO.putStrLn $ "Part 1: " <> T.show result1
    TIO.putStrLn $ "Part 2: " <> T.show result2
