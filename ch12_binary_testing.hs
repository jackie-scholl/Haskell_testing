module BinaryTesting where
    
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)


unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = temp $ f x where
    temp Nothing = Leaf
    temp (Just (a1, b, a2)) = Node (unfold f a1) b (unfold f a2)
        


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
    where f x
            | x == n = Nothing
            | otherwise = Just (x+1, x, x+1)