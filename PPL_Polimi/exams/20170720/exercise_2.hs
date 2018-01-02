module Exercise2(yellowSubTrees, main) where

-- 1) Define a ternary tree data structure, called Ttree, in which every
-- node contain both a value and a color, which can be either
-- yellow or blue. You can assume that a tree cannot be empty.

data Color = Yellow | Blue deriving (Show)

data Ttree a = Leaf a Color
             | Node a Color (Ttree a) (Ttree a) (Ttree a) 
             deriving (Show)

-- 2) Make Ttree an instance of Functor.

instance Functor Ttree where
    fmap f (Leaf a c) = Leaf (f a) c
    fmap f (Node a c t1 t2 t3) = Node (f a) c (f <$> t1) (f <$> t2) (f <$> t3)

-- 3) Make Ttree an instance of Foldable.

-- foldr :: Foldable f => (a -> b -> b) -> b -> f a -> b
instance Foldable Ttree where
    foldr f start (Leaf a _) = f a start
    foldr f start (Node a _ t1 t2 t3) = let x = foldr f start t3
                                            x' = foldr f x t2
                                            x'' = f a x'
                                        in foldr f x'' t1

-- 4) Define a function yellowSubTrees, which returns a list containing all
-- the maximal subtrees of a given Ttree that are all made of yellow nodes.
-- E.g. in the case of the following tree, where yellow nodes are depicted in
-- white, the returned list contains the three outlined subtrees.

isYellow :: Ttree a -> Bool
-- A leaf is Yellow trivially if its color is Yellow
isYellow (Leaf _ Yellow) = True
isYellow (Leaf _ _) = False

-- A Node is Yellow if its color is yellow and both its children are Yellow
isYellow (Node _ Yellow t1 t2 t3) = isYellow t1 && isYellow t2 && isYellow t3
isYellow Node{} = False


yellowSubTrees :: Ttree a -> [Ttree a]
yellowSubTrees t
    | isYellow t = [t]
    | otherwise = case t of
                    (Node _ _ t1 t2 t3) -> concatMap yellowSubTrees [t1, t2, t3]
                    (Leaf _ _) -> []

main :: IO()
main = do
    let t1 = Node 1 Blue
                (Node 2 Yellow
                    (Leaf 3 Yellow)
                    (Leaf 4 Yellow)
                    (Leaf 5 Yellow))
                (Leaf 6 Blue)
                (Node 7 Yellow
                    (Leaf 8 Yellow)
                    (Leaf 9 Blue)
                    (Leaf 10 Yellow))
    print (yellowSubTrees t1)
