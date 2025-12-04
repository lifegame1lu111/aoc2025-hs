{-# LANGUAGE LambdaCase #-}

import Data.List (mapAccumL)
import Data.Function ((&))
import Data.Bool (bool)

applyRotation :: Int -> String -> Int
applyRotation dial = \case
    'L' : dist -> dial - read dist
    'R' : dist -> dial + read dist
    _ -> 0

part1 :: [String] -> Int
part1 rots = mapAccumL go 50 rots
             & snd
             & filter (== 0)
             & length
  where
    go :: Int -> String -> (Int, Int)
    go dial rot = 
        let dial' = applyRotation dial rot `mod` 100
        in (dial', dial')    

part2 :: [String] -> Int
part2 rots = 
    let (_, states) = mapAccumL go 50 rots           
    in sum states
  where
    go :: Int -> String -> (Int, Int)
    go dial rot =
        let rawRot = applyRotation dial rot
            c = abs rawRot `quot` 100
            dial' = rawRot `mod` 100
            comp = bool 0 1 $ rawRot <= 0 && dial /= 0
        in (dial', comp + c)

main :: IO ()
main = do
    input <- lines <$> readFile "input01.txt"

    let result1 = part1 input
    let result2 = part2 input

    putStrLn $ "Part 1: " <> show result1
    putStrLn $ "Part 2: " <> show result2
