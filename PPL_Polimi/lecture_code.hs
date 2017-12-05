module LectureCode (main) where

import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Char as Char

-- Length of a list
-- This is the first example of pattern matching. Arguments
-- are matched with the right parts of equations, top to
-- bottom. If a match is found the function body is called.
-- 
-- This is also an example of parametric polimorphism, what
-- in other languages is implemented with "generics" in Java
-- or "templates" in C++. Here [a] stands for "a list of
-- elements of type a, for any a".
llen :: [a] -> Integer
llen [] = 0
llen (_:xs) = 1 + llen xs

-- User defined types are based on 'data declarations'
--
-- a "sum" type (union in C).
-- in this case MyBool is the (nullary) type ctor while False
-- and True are the (again nullary) data ctors.
-- The "kind" of a nullary type ctor is '*'
data MyBool = False | True

-- a "product" type (struct in C)
-- It resambles a ctor of a class with two fields of type
-- 'a'. By the way, type ctors and data ctors live in two
-- separate namespaces so no name clashing.
-- Warning: there's no ctor overloading.
data Pnt a = Pnt a a

-- A mixed type (something real)
data MyTree a = Leaf a | Branch (MyTree a) (MyTree a)

-- An example of function in Haskell.
-- Take the fringe of a tree and return it as a list
-- or in Haskell function type:
fringe :: MyTree a -> [a]
fringe (Leaf a) = [a]
fringe (Branch l r) = fringe l ++ fringe r

-- MyListCat
lcat :: [a] -> [a] -> [a]
lcat l [] = l
lcat [] l = l
lcat (x:xs) l = x : lcat xs l

-- Luckily we have a sane syntax for declaring types, their
-- fields and access them easily
data Point = Point {pointx, pointy :: Float}

-- Typical functional functions
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- Call-by-need is particularly convenient to handle
-- never-ending computations that provide data
ones = 1 : ones
numsFrom n = n : numsFrom (n + 1)
squares = map (^2) (numsFrom 0)
fib = 1 : 1 : [a+b | (a, b) <- zip fib (tail fib)]

-- Folding left and Folding right
myFoldL :: (b -> a -> b) -> b -> [a] -> b
myFoldL _ b [] = b
myFoldL f b (a:as) = myFoldL f (f b a) as

myFoldR :: (a -> b -> b) -> b -> [a] -> b
myFoldR _ b [] = b
myFoldR f b (a:as) = f a (myFoldR f b as)

-- So we know haskell is lazily evaluated, so we don't
-- take advantage of tail-recursive code. How to force
-- strict evaluation? Use seq :: a -> t -> t. It returns
-- t iff evaluation of a terminates.
myFoldL' :: (b -> a -> b) -> b -> [a] -> b
myFoldL' _ b [] = b
myFoldL' f b (a:as) = let b' = f b a
                      in seq b' (myFoldL' f b' as)

-- Type classes are used for overloading: a class is a
-- container of overloaded operations. We declare a type
-- to be an instance of a type class, meaning that it
-- implements its operations
--
-- In Java an Object is an instance of a Class
-- In Haskell a Type is an instance of a Class.
-- Haskell      Java
-- -----------------------
-- Class        Interface
-- Type         Class
-- Value        Object
-- Method       Method
class MyEq a where
    (===) :: a -> a -> Bool

-- MyOrd is said to be a subclass of MyEq
-- Note: in Prelude's Ord we only have to define (<=)
-- and all the others are derived from it (which is
-- possible because we are subclassing Eq as well)
class (MyEq a) => MyOrd a where
    lt, lte, gte, gt :: a -> a -> Bool

instance (Eq a) => MyEq (MyTree a) where
-- Type a must support equality as well
    (Leaf v1) === (Leaf v2) = v1 == v2
    (Branch l1 r1) === (Branch l2 r2) = (l1 === l2) &&
        (r1 === r2)
    _ === _ = Prelude.False

instance Show a => Show (MyTree a) where
    show (Leaf a) = show a
    show (Branch l r) = "<" ++ show l ++
                        " | " ++ show r ++ ">"

-- Tree is foldable
instance Foldable MyTree where
    foldr f z (Leaf x) = f x z
    foldr f z (Branch l r) = foldr f (foldr f z r) l

-- Functor is a class of all the types that offer a map
-- operation. The map operation of functors is called
-- 'fmap' has type:
-- fmap :: (Functor f) => (a -> b) -> f a -> f b
instance Functor MyTree where
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Branch a b) = Branch (fmap f a) (fmap f b)

nextChar :: Char -> Char
nextChar c = Char.chr (Char.ord c + 1)

-- Example on Monadic function
exmonad :: (Monad m, Num r) => m r -> m r -> m r
exmonad m1 m2 = do x1 <- m1
                   x2 <- m2
                   return $ x1 - x2

main :: IO()
main = do
    putStrLn "Haskell Lectures Code"
    let list = [1, 2, 3, 4, 5]
    putStrLn $ "list = " ++ show list
    putStrLn $ "Len(list) = " ++ show (llen list)
    putStrLn $ "Cat(list, list) = " ++ show (lcat list list)
    putStrLn $ "Map((+1), list) = " ++ show (myMap (+1) list)
    
    -- You can only instantiate a MyTree with data ctors
    -- You can't use MyTree because it's a type ctor, like
    -- an abstract class ctor.
    let aTree = Branch (Leaf 'a')
                       (Branch (Leaf 'b') (Leaf 'c'))
    let bTree = Branch (Leaf 'a')
                       (Branch (Leaf 'b') (Leaf 'd'))
    putStrLn $ "aTree = " ++ show aTree
    putStrLn $ "bTree = " ++ show bTree
    putStrLn $ "fringe aTree: " ++ fringe aTree
    putStrLn $ "fringe bTree: " ++ fringe bTree
    putStrLn $ "aTree === bTree: " ++ show (aTree === bTree)
    putStrLn $ "aTree === aTree: " ++ show (aTree === aTree)

    let cTree = fmap nextChar aTree
    putStrLn $ "fmap nextChar ( " ++ show aTree ++
               " ) = " ++ show cTree

    -- Get the fringe by folding
    putStrLn $ "foldr (:) \"\" aTree = " ++ foldr (:) "" aTree 

    -- Simple test of a point in R^2
    let p = Point 2.0 3.0
    putStrLn $ "p {x: " ++ show (pointx p) ++
               ", y: " ++ show (pointy p) ++ "}"

    -- Test some infinite lists
    putStrLn $ "Take 5 ones: " ++ show (take 5 ones)
    putStrLn $ "Take 5 squares: " ++ show (take 5 squares)
    putStrLn $ "Take 10 fibonaccis: " ++ show (take 10 fib)

    -- Trying out zip and list comprehensions:
    putStrLn $ "zip [1,2,3] \"ciao\" = " ++
                show (zip [1,2,3] "ciao")
    putStrLn $ "[(x, y) | x <- [1, 2], y <- \"ciao\"] =\n\t" ++
                show [(x, y) | x <- [1, 2], y <- "ciao"]

    -- Playing around with let/where expressions
    let x = 3
        y = 2
        in print (x + y)
    -- or equivantely
    let {x = 3; y = 2} in print (x + y)

    -- Usual, practical data strutures:
    -- Data.Map (immutable)
    let exmap = let m = M.fromList [("nose", 11), ("emerald", 27)]
                    n = M.insert "rug" 98 m
                    o = M.insert "nose" 9 n
                in (m M.! "emerald", n M.! "rug", o M.! "nose")
    putStrLn $ "(m ! \"emerald\", n ! \"rug\", " ++ 
               "o ! \"nose\") = " ++ show exmap
    -- Data.Array (immutable! Each update performs a copy)
    let exarray = let m = A.listArray (1, 3) ["alpha",
                                              "beta",
                                              "gamma"]
                      n = m A.// [(2, "Beta")]
                      o = n A.// [(1, "Alpha"),
                                  (3, "Gamma")]
                  in (m A.! 1, n A.! 2, o A.! 1)
    putStrLn $ "(m ! 1, n ! 2, o ! 1) = " ++ show exarray
    do {
        x <- exmonad (do {
                        putStr "?> ";
                        x <- getLine;
                        return (read x :: Int);
                      })
                     (return 10);
        print x;
    }