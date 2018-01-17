class YesNo a where
    yesno :: a -> Bool

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Integer where
    yesno 0 = False
    yesno _ = True

instance YesNo Bool where
--    yesno True = True
--    yesno False = False
    yesno = id

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal then yesResult else noResult
