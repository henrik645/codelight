import Data.Maybe (mapMaybe)
import Data.List (foldl')

data Item
    = Paren
    | Brace
    | Bracket
    deriving (Eq, Show)

data Command
    = Push Item
    | Pop Item

type Stack = [Item]

data State = State
    { isValid :: Bool
    , stack :: Stack
    , nPairs :: Int
    }
    deriving (Show)

initialState :: State
initialState = State { isValid = True, stack = [], nPairs = 0 }

handle :: State -> Command -> State
handle state@(State { isValid, stack, nPairs }) command =
    if not isValid then
        state -- don't do anything to state
    else
        case command of
            Push item -> state { stack = item : stack } -- add item to top of stack
            Pop item -> 
                case stack of
                    (first:rest) | first == item -> 
                        -- close a pair, continue with rest of stack
                        state { stack = rest, nPairs = nPairs + 1 }
                    _ ->
                        -- else, either empty stack or nesting mismatch 
                        state { isValid = False }

charToCommand :: Char -> Maybe Command
charToCommand '(' = Just $ Push Paren
charToCommand ')' = Just $ Pop Paren
charToCommand '[' = Just $ Push Bracket
charToCommand ']' = Just $ Pop Bracket
charToCommand '{' = Just $ Push Brace
charToCommand '}' = Just $ Pop Brace
charToCommand _   = Nothing

count :: State -> Int
count (State { isValid, nPairs }) =
    if isValid then
        nPairs + 1
    else
        nPairs
        
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let commands = mapMaybe charToCommand contents
    let result = foldl' handle initialState commands
    print $ count result
