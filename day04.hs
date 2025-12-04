import Data.Maybe (isJust, mapMaybe)
import Data.List ((!?))
import Data.Bifunctor (bimap)
import Data.Function ((&))

type Grid = [String]
type Point = (Int, Int)

getGridSize :: Grid -> (Int, Int)
getGridSize grid@(row : _) = (length row, length grid)
getGridSize _ = (0, 0)

part1 :: Grid -> [(Int, Int)]
part1 grid = [(getAdjRollCount (x, y), (x, y), (grid !! y) !! x) 
             | x <- [0..xMax - 1]
             , y <- [0..yMax - 1]
             ]
    & filter (\(count, _, tile) -> count < 4 && tile == '@')
    & map (\(_, coord, tile) -> coord)
  where
    xMax, yMax :: Int
    (xMax, yMax) = getGridSize grid

    getAdjRollCount :: Point -> Int
    getAdjRollCount (x, y) = deltas
        & mapMaybe (tryGetTile . bimap (x +) (y +))
        & filter (== '@')
        & length

    deltas :: [Point]
    deltas = [ (0, 1)
             , (0, -1)
             , (1, 0)
             , (-1, 0)
             , (1, 1)
             , (-1, 1)
             , (1, -1)
             , (-1, -1)
             ]

    tryGetTile :: Point -> Maybe Char
    tryGetTile (x, y) = case grid !? y of
        Just row -> row !? x
        Nothing -> Nothing

part2 :: [String] -> Int
part2 grid = case part1 grid of
    [] -> 0
    points -> let go :: Point -> Char -> Char
                  go point tile = if point `elem` points
                                  then '.'
                                  else tile
                  grid' = [ [ go (x, y) $ (grid !! y) !! x
                            | x <- [0..xMax - 1]
                            ]
                          | y <- [0..yMax - 1]
                          ]
              in length points + part2 grid'

  where
    xMax, yMax :: Int
    (xMax, yMax) = getGridSize grid
    
main :: IO ()
main = do
    grid <- lines <$> readFile "input04.txt"

    let result1 = length $ part1 grid
        result2 = part2 grid

    putStrLn $ "Part 1: " <> show result1
    putStrLn $ "Part 2: " <> show result2
