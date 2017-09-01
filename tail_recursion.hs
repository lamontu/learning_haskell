-- In C, we use mutable variables to create loops:
-- long factorial (int n) {
--   long result = 1;
--   while (n > 1) {
--     result *= n--;
--   }
--   return result;
-- }  

-- In Haskell, use recursion to "re-bind" argument symbols in new scope
-- factorial n = if n > 1
--               then n * factorial (n - 1)
--               else 1

-- Use accumulator argument to make calls tail recursive
factorial n = let loop acc n' = if n' > 1
                                then loop (acc * n') (n' - 1)
                                else acc
              in loop 1 n
main = print (factorial 5)
