import Data.Char
import Data.List

main = do
--    line <- getLine
--    let line' = reverse line
    --line' <- fmap reverse getLine -- same as above

    line' <- fmap (intersperse '-' . reverse . map toUpper) getLine

    putStrLn $ "You said " ++ line' ++ " backwards!"
    putStrLn $ "Yes, you really said " ++ line' ++ " backward!"

data CMaybe a = CNothing | CJust Int a deriving (Show)

-- Not a functor instance
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

myAction :: IO String
-- myAction = do
--     a <- getLine
--     b <- getLine
--     return $ a ++ b
myAction = (++) <$> getLine <*> getLine
