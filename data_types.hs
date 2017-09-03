
data Point = Point Double Double deriving Show
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
