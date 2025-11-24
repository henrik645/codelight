import Data.List (elemIndex, findIndex)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import Data.Set qualified as Set

data Tile = Obstacle | Path | Start | End deriving (Eq)

instance Show Tile where
  show Obstacle = "#"
  show Path = "."
  show Start = "S"
  show End = "E"

newtype Map = Map [[Tile]] deriving (Show)

parseTile :: Char -> Tile
parseTile '.' = Path
parseTile '#' = Obstacle
parseTile 'S' = Start
parseTile 'E' = End
parseTile x = error $ "Invalid tile: " ++ show x

parseMap :: [String] -> Map
parseMap = Map . fmap (fmap parseTile)

getTile :: (Int, Int) -> Map -> Tile
getTile (x, y) (Map tiles) = (tiles !! y) !! x

find :: Tile -> Map -> (Int, Int)
find tile (Map tiles) = (x, y)
  where
    test = fmap (elemIndex tile) tiles
    y = fromJust $ findIndex isJust test
    x = fromJust $ test !! y

data State = State
  { currentPos :: (Int, Int),
    energy :: Int,
    visited :: Set (Int, Int)
  }
  deriving (Show)

data Direction = East | West | North | South deriving (Show)

-- calculate energy to follow a path given as a list of coordinates
energyBetween :: Direction -> [(Int, Int)] -> Int
energyBetween direction points = sum $ zipWith go points (tail points)
  where
    go (x1, y1) (x2, y2) = undefined

dimensions :: Map -> (Int, Int)
dimensions (Map tiles) = (length $ head tiles, length tiles)

initialState :: Map -> State
initialState map = State {currentPos = find Start map, energy = 0, visited = Set.empty}

possibleSteps :: (Int, Int) -> [(Int, Int)]
possibleSteps (x, y) =
  [ (x + 1, y),
    (x, y + 1),
    (x - 1, y),
    (x, y - 1)
  ]

isValidStep :: (Int, Int) -> Map -> State -> Bool
isValidStep (x, y) map state =
  let (maxX, maxY) = dimensions map
   in x >= 0 && x <= maxX && y >= 0 && y <= maxY && getTile (x, y) map `elem` [Path, End] && (x, y) `notElem` visited state

atEnd :: State -> Map -> Bool
atEnd state map = getTile (currentPos state) map == End

inRange max x = x >= 0 && x <= max

nextSteps :: State -> Map -> [State]
nextSteps state map
  | atEnd state map = []
  | otherwise = fmap (\n -> state {currentPos = n, visited = Set.insert (currentPos state) (visited state)}) newVisited
  where
    newVisited = filter isValid $ possibleSteps (currentPos state)
    (maxX, maxY) = dimensions map
    isValid (x, y) = isValidStep (x, y) map state

main = do
  map <- parseMap . lines <$> readFile "input.txt"
  putStrLn $ "Dimensions: " ++ show (dimensions map)
  putStrLn $ "Start: " ++ show (find Start map)
  putStrLn $ "End: " ++ show (find End map)
  let is = initialState map
  let results = concatMap (filter (`atEnd` map)) (takeWhile (not . null) $ iterate (\ss -> ss >>= (\s -> nextSteps s map)) [is])
  mapM_ print results
