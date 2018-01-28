main = do
    putChar 't'
    putChar 'e'
    putChar 'h'
    putChar '\n'

    putStr "Hey, "
    putStr "I'm "
    putStrLn "Andy!"

    return ()
    -- a <- return "HAHAHA"
    line <- getLine
    -- b <- return "BLAH BLAH BLAH"
    return 4
    let a = "HAHAHA"
        b = "BLAH BLAH BLAH"
    putStrLn $ line ++ " "++ a ++ " " ++ b

