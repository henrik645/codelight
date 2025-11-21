inputFile :: String
inputFile = "input.txt"

main :: IO ()
main = do
    content <- fmap lines $ readFile inputFile
    print $ number isPalindrome $ map removeNonEnglish content

number :: (a -> Bool) -> [a] -> Int
number p = length . filter p
        
removeNonEnglish :: String -> String
removeNonEnglish = filter (`elem` ['a'..'z'])

isPalindrome :: String -> Bool
isPalindrome str = reverse str == str
