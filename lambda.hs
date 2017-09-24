import Data.Char

countLowerCase :: String -> Int
-- countLowerCase str = length (filter isLower str)
countLowerCase = length . filter isLower

countLowercaseAndDigits :: String -> Int
countLowercaseAndDigits =
    length . filter (\c -> isLower c || isDigit c)
