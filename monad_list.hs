class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)

instance MonadPlus Maybe where
    mzero = Nothing
    mplus (Just a) Nothing = Just a
    mplus Nothing (Just a) = Just a
    mplus _ _ = Nothing

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

sevenOnly :: [Int]
sevenOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x


type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [ (c+2, r+1), (c+1, r+2), (c-1, r+2), (c-2, r+1)
                 ,(c-2, r-1), (c-1, r-2), (c+1, r-2), (c+2, r-1)
                ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
