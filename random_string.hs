import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen)
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen) -- Same result

    putStrLn "----------------"

    let randomChars = randomRs ('a', 'z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _) = splitAt 20 rest
    putStrLn first20
    putStrLn second20

    putStrLn "----------------"

    gen' <- getStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen')
    gen'' <- newStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen'')
