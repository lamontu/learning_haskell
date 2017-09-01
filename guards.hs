-- let in: guards are evaluated top to bottom, the firs True guard wins
-- factorial n = let loop acc n' | n' > 1 = loop (acc * n') (n' - 1)
--                               | otherwise = acc
--               in loop 1 n

-- where clauses
factorial n = loop 1 n
  where loop acc n' | n' > 1 = loop (acc * n') (n' - 1)
                    | otherwise = acc
              
main = print (factorial 5)

