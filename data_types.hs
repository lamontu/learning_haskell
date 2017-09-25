import System.IO

data Point = Point Double Double deriving (Show, Read)
myPoint :: Point
myPoint = Point 1.0 2.0

data Color = Red | Green | Blue | Violet deriving (Show, Eq, Enum)
myColor :: Color
myColor = Red
getX, getMaxCoord :: Point -> Double
getX point = case point of
               Point x y -> x
getMaxCoord (Point x y) | x > y     = x
                        | otherwise = y
isRed :: Color -> Bool
isRed Red = True
isRed c   = False

-- main = print (getX myPoint)
-- main = print (getMaxCoord myPoint)
-- main = print (isRed myColor)

data Move = Rock | Paper | Scissors
     deriving (Eq, Read, Show, Enum, Bounded)
data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)
outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
                | otherwise  = Lose

main = print (outcome Rock Paper)

parseMove :: String -> Maybe Move
parseMove str = case reads str of 
  [(m, rest)] | ok rest -> Just m
  _                     -> Nothing
  where ok = all (`elem` " \r\n")


withTty = withFile "/dev/tty" ReadWriteMode

getMove :: Handle -> IO Move
getMove h = do
  hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
  input <- hGetLine h
  case parseMove input of Just move -> return move
                          Nothing -> getMove h

computerVsUser :: Move -> Handle -> IO ()
computerVsUser computerMove h = do
  userMove <- getMove h
  let o = outcome userMove computerMove
  hPutStrLn h $ "You " ++ show o

-- GHCi data_types.hs
-- withTty $ computerVsUser Rock
