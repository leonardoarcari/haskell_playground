module Exercise2(tcompose, Exercise2.reverse,
                 main) where

import qualified Data.List as List
-- Consider the following binary tree data structure:

data Tree a = Nil
            | Leaf a 
            | Branch (Tree a) (Tree a)
            deriving (Show, Eq)

-- 1) Define a tcompose operation, which takes a function f and two trees,
--    t1 and t2, and returns a tree with the same structure as t1,
--    but with leaves replaced by subtrees having the same structure of t2:
--    each leaf is obtained by applying f to the value stored in the
--    previous leaf, and the corresponding value in t2.
-- E.g. 
--    t1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)
--    t2 = Branch (Leaf 6) (Leaf 7)
-- tcompose (+) t1 t2 is
--    Branch (Branch 
--             (Branch (Leaf 7) (Leaf 8))
--             (Branch (Leaf 8) (Leaf 9)))
--           (Branch (Leaf 9) (Leaf 10))

tcompose :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
tcompose _ Nil _ = Nil
tcompose _ _ Nil = Nil
tcompose f (Leaf a) (Leaf b) = Leaf $ f a b
tcompose f t1@(Leaf _) (Branch l r) = Branch (tcompose f t1 l) (tcompose f t1 r)
tcompose f (Branch l r) t2 = Branch (tcompose f l t2) (tcompose f r t2)

-- 2) Define a purely functional (i.e. non destructive) version of the
--    reverse presented in Es. 1.3.
--    E.g. revtree t1 is Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1).

fringe :: Tree a -> [a]
fringe Nil = []
fringe (Leaf a) = [a]
fringe (Branch l r) = fringe l ++ fringe r

reverse :: Tree a -> Tree a
reverse t = fst $ reverse' t xs where
    xs = List.reverse $ fringe t
    reverse' Nil ys = (Nil, ys)
    reverse' (Leaf _) [] = error "Unable to reverse this Tree."
    reverse' (Leaf _) (y:ys) = (Leaf y, ys)
    reverse' (Branch l r) ys = let (left, zs) = reverse' l ys
                                   (right, zs') = reverse' r zs
                               in (Branch left right, zs') 

main :: IO()
main = do
    let t1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)
    let t2 = Branch (Leaf 6) (Leaf 7)

    putStrLn $ "t1 = " ++ show t1
    putStrLn $ "t2 = " ++ show t2
    print "Applying \"tcompose (+) t1 t2\"..."
    print $ tcompose (+) t1 t2

    print "\nReversing t1..."
    print $ Exercise2.reverse t1