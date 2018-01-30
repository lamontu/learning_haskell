import Control.Monad

main = do
    mapM print [1..5]
    print "----------------"
    mapM_ print [1..5]
    print "----------------"

    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a
            ++ "?" -- need indent
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
