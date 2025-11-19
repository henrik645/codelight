import Control.Monad (foldM)

matching ')' = '('
matching ']' = '['
matching '}' = '{'

handle :: Char -> ([Char], Int) -> Either Int ([Char], Int)
handle c (stack, pairs) =
    case c of
        _ | c `elem` ['(', '[', '{'] -> Right (c : stack, pairs)
        _ | c `elem` [')', ']', '}'] ->
            case stack of
                (first:rest) | first == matching c -> Right (rest, pairs + 1)
                _ -> Left pairs
        _ -> Right (stack, pairs)

count (Left n) = n
count (Right (_, n)) = n + 1

main = do
    content <- readFile "input.txt"
    let result = foldM (flip handle) ([], 0) content
    print $ count result

