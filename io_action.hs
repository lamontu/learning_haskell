import Data.Char

--main = do
--    -- foo <- putStrLn "Hello, what's your name?"
--    putStrLn "Hello, what's your first name?"
--    firstName <- getLine
--    putStrLn "Hello, what's your last name?"
--    lastName <- getLine
--    let bigFirstName = map toUpper firstName
--        bigLastName = map toUpper lastName
--    putStrLn ("Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", you rock!")

main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
