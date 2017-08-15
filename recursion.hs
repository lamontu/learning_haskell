import Data.Char

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
