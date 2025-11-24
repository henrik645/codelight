import Data.Ord (comparing)
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set

data Time = Time Int Int deriving (Eq, Show)

instance Ord Time where
    (Time h1 m1) <= (Time h2 m2) = (h1 < h2) || (h1 == h2 && m1 <= m2)

data EventType = In | Out deriving (Show, Eq, Ord)

newtype Person = Person String deriving (Eq, Show, Ord)

data Event = Event
    { eventType :: EventType
    , person :: Person
    , time :: Time
    }
    deriving (Eq, Show)

instance Ord Event where
    compare = comparing time <> comparing eventType

parseTime :: String -> Time
parseTime [h1, h2, ':', m1, m2] = Time (read [h1, h2]) (read [m1, m2])
parseTime input = error $ "invalid time: " ++ input

parseEventType :: String -> EventType
parseEventType "IN" = In
parseEventType "OUT" = Out
parseEventType input = error $ "invalid event: " ++ input

parseEvent :: String -> Event
parseEvent input =
    let parts = words input
        time = parseTime $ parts !! 0
        person = Person $ parts !! 1
        eventType = parseEventType $ parts !! 2
     in Event { eventType = eventType, person = person, time = time }

type Duration = (Time, Time)

minutes :: Time -> Time -> Int
minutes (Time h1 m1) (Time h2 m2) = (h2 - h1) * 60 + (m2 - m1)

data State = NotThree Time | Three {- since -} Time deriving (Show)

data Bistro = Bistro
    { people :: Set Person -- people in the bistro
    , state :: State
    } deriving (Show)

initialBistro :: Bistro
initialBistro = Bistro
    { people = Set.empty
    , state = NotThree (Time 0 0)
    }

handleEvent :: Bistro -> Event -> Bistro
handleEvent bistro event =
    let operation =
            case eventType event of
                In -> Set.insert
                Out -> Set.delete
        bistro' = bistro { people = operation (person event) (people bistro) }
        nPeople = length $ people bistro'
        newState =
            case (state bistro, nPeople) of
                (Three since, 3) -> Three since
                (NotThree _, 3)    -> Three (time event)
                _                -> NotThree (time event)
     in bistro' { state = newState }

calculateDurations :: [(State, State)] -> [Int]
calculateDurations = map go
    where
        go (Three start, NotThree end) = minutes start end
        go _ = 0

main :: IO ()
main = do
    events <- fmap (map parseEvent . lines) $ readFile "input.txt"
    let sorted = sort events
    let allResults = scanl handleEvent initialBistro sorted
    let states = map state allResults
    let pairs = zip states (tail states)
    let durations = calculateDurations pairs
    print $ sum durations
