import Data.Char

type Point = (Int, Int)
orgin :: Point
orgin = (0, 0)
-- move a given point to the right
moveRight :: Point -> Int -> Point
moveRight (x, y) distance = (x + distance, y)
-- move a given point to upwards
moveUp :: Point -> Int -> Point
moveUp (x, y) distance = (x, y + distance)

type Colour = String
type ColourPoint = (Int, Int, Colour)
origin :: Colour -> ColourPoint
origin colour = (0, 0, colour)

-- move a colour point vertically and horizontally
move :: ColourPoint -> Int -> Int -> ColourPoint
move (x, y, colour) xDistance yDistance
  = (x + xDistance, y + yDistance, colour)

-- compute the distance between two colour points
distance :: ColourPoint -> ColourPoint -> Float
distance (x1, y1, colour1) (x2, y2, colour2)
  = sqrt (fromIntegral (dx * dx + dy * dy))
  where
    dx = x2 - x1
    dy = y2 - y1

natSum :: (Num a, Ord a) => a -> a
natSum 0 = 0
natSum n | n > 0 = n + natSum (n - 1)
         | otherwise = error "natSum: Input value too small!" 

repeatN :: Int -> a -> [a]
repeatN 0 x = []
repeatN n x = x : repeatN (n - 1) x 

suffixes :: String -> [String]
suffixes ""  = []
suffixes str = str : suffixes (tail str)

allSquares :: Num a => [a] -> [a]
allSquares []       = []
allSquares (x : xs) = x * x : allSquares xs

allToUpper :: String -> String
allToUpper []                 = []
allToUpper (chr : restString) = toUpper chr : allToUpper restString

extractDigits :: String -> String
extractDigits []
  = []
extractDigits (chr : restString)
  | isDigit chr = chr : extractDigits restString
  | otherwise   = extractDigits restString
