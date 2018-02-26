type Birds = Int
type Pole = (Birds, Birds)

-- landLeft :: Birds -> Pole -> Pole
-- landLeft n (left, right) = (left + n, right)
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

-- landRight :: Birds -> Pole -> Pole
-- landRight n (left, right) = (left, right + n)
landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing


routine = return (0, 0) >>= landLeft 2 
    -- >> Nothing
    >>= landRight 2 >>= landLeft 1

-- Use do syntax for many monad value (same as above) 
routine' :: Maybe Pole
routine' = do
    start <- return (0, 0)
    first <- landLeft 2 start
    -- Nothing
    second <- landRight 2 first
    landLeft 1 second
