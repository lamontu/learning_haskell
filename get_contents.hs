import Data.Char

-- main = do
--     contents <- getContents
--     putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input = 
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result

-- main = interact $ unlines . filter ((<10) . length) . lines

respondPalindromes contents = unlines (map (\xs ->
    if isPalindrome xs then "palindrome" else "not a palindrome")
        (lines contents))
    where isPalindrome xs = xs == reverse xs

main = interact respondPalindromes
