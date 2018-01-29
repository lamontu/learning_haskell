import Control.Monad

--main = do
--    c <- getChar
--    if c /= ' '
--        then do
--            putChar c
--            main
--        else return ()

--    when (c /= ' ') $ do
--        putChar c
--        main

--------------------------------

main = do
--    a <- getLine
--    b <- getLine
--    c <- getLine
--    print [a,b,c]

    rs <- sequence [getLine, getLine, getLine]
    print rs
