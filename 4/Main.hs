tick :: [Integer] -> [Integer]
tick [zero, one, two, three, four, five, six, seven, eight, nine] =
    -- 0  1    2      3     4     5    6             7      8     9
    [one, two, three, four, five, six, seven + zero, eight, nine, zero]

main = do
    let people = [1, 0, 1, 2, 0, 0, 1, 0, 0, 0] :: [Integer]
    let ticks = iterate tick people
    print $ sum $ ticks !! 365

