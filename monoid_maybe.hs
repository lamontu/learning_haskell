-- instance Monoid a => Monoid (Maybe a) where
--     mempty = Nothing
--     Nothing `mappend` m = m
--     m `mappend` Nothing = m
--     Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

newtype First a = First {getFirst :: Maybe a}
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x

newtype Last a = Last {getLast :: Maybe a}
    deriving (Eq, Ord, Read, Show)

instance Monoid (Last a) where
    mempty = Last Nothing
    _ `mappend` Last (Just x) = Last (Just x)
    x `mappend` Last Nothing = x
