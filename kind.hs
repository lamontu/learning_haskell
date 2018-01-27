class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

fr = Frank {frankField = Just "haha"}
fs = Frank {frankField = "yes"}

instance Tofu Frank where
    tofu x = Frank x

-- ghci kind.hs
-- :t fr
-- :t fs
-- tofu (Just 'a') :: Frank Char Maybe
-- tofu "hello" :: Frank Char []
