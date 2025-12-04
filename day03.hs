import Data.List
import Data.Function ((&))

part1 :: [String] -> Int
part1 = sum . map go
  where
    go :: String -> Int
    go bank = let rs = read . (: []) <$> bank
                  d1 = maximum . drop 1 . reverse $ rs
                  d1Idx = head $ elemIndices d1 rs
                  d2 = maximum . drop (d1Idx + 1) $ rs
              in 10 * d1 + d2

part2 :: [String] -> Int
part2 = sum . map go
  where
    go :: String -> Int
    go bank = let rs = read . (: []) <$> bank
                  (_, ds) = mapAccumL (go' rs) (-1) range
              in sum $ zipWith (*) ds (map (10 ^) range)

    go' :: [Int] -> Int -> Int -> (Int, Int)
    go' rs idx skip = let rs' = rs
                                & reverse 
                                & drop skip
                                & reverse
                                & drop (idx + 1)
                          d = maximum rs'
                          dIdx = head $ elemIndices d rs'
                      in (idx + dIdx + 1, d)

    range :: [Int]
    range = reverse [0..11]

main :: IO ()
main = do
    content <- lines <$> readFile "input03.txt"

    let result1 = part1 content
        result2 = part2 content

    putStrLn $ "Part 1: " <> show result1
    putStrLn $ "Part 2: " <> show result2
