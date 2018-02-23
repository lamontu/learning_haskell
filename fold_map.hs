import RecursiveTree 
import MonoidAnyAll

instance Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = foldMap f l
                            `mappend` f x
                            `mappend` foldMap f r

testTree = Node 5
            (Node 3
                (Node 1 EmptyTree EmptyTree)
                (Node 6 EmptyTree EmptyTree)
            )
            (Node 9
                (Node 8 EmptyTree EmptyTree)
                (Node 10 EmptyTree EmptyTree)
            )

testList = [1,2,3,4,5]

test1 = map (\x -> Any $ x == 3) testList
test2 = foldMap (\x -> Any $ x == 3) testTree
test3 = foldMap (\x -> Any $ x > 15) testTree
test4 = foldMap (\x -> [x]) testTree
