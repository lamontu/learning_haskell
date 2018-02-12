class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
    fmap' = map

instance Functor' Maybe where
    fmap' f (Just x) = Just (f x)
    fmap' f Nothing = Nothing

instance Functor' (Either a) where
    fmap' f (Right x) = Right (f x)
    fmap' f (Left x) = Left x

data Barry t k p = Barry { yabba :: p, dabba :: t k } deriving (Show)

instance Functor' (Barry a b) where
    fmap' f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

-- ghci functor.hs
-- fmap' (*3) (Barry {yabba = 2, dabba = Just 'a'})

instance Functor' IO where
    fmap' f action = do
        result <- action
        return (f result)

instance Functor' ((->) r) where
    fmap' f g = (\x -> f (g x))
