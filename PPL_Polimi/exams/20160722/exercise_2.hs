module Exercise2(mIterate) where

-- 1) Define iter which works like iterate in the previous exercise.
-- Note that Haskell already has iterate, but of course you cannot
-- use it to define iter.

mIterate :: (a -> a) -> a -> [a]
mIterate f v = v : mIterate f (f v)

-- 2) Consider this data type:

data Rf a b = Rf [a] (a -> b)

-- Its first component is a list of values representing
-- the domain of the second argument (a function).
-- This means that the values returned from the function are
-- meaningful only if its parameter is taken from the first list.
-- Is it possible to derive Show? Why? If the answer is no, make
-- Rf an instance of Show, so that e.g. Rf [1,2,3]
-- (+1) is represented as: “[1,2,3]-->[2,3,4]”
-- (notice that [2,3,4] is the image of (+1) on the given domain).

instance (Show a, Show b) => Show (Rf a b) where
    show (Rf dom f) = show dom ++ "-->" ++ (show $ map f dom)

-- 3) Make (Rf a) an instance of Functor.

instance Functor (Rf a) where
    fmap f (Rf dom g) = Rf dom (f . g)

-- 4) The Rf data type is used to represent functions. Given two
-- Rf input values, say of type
-- (Rf a b) and (Rf b c),
-- define a way to compose functions, i.e. write a function compose
-- which returns a value of type (Rf a c).
-- Write the type of compose.
-- E.g. compose (Rf [1,2,3] (+2)) (Rf [2,3,5] (*2))
-- should be
-- Rf [1,3] (\x -> (x+2)*2).

fixDomain :: (Eq b) => [a] -> (a -> b) -> [b] -> [a]
fixDomain [] _ _ = []
fixDomain (x:xs) f b = if (elem (f x) b)
                        then x : fixDomain xs f b
                        else fixDomain xs f b

compose :: (Eq b) => Rf a b -> Rf b c -> Rf a c
compose (Rf dom1 b) (Rf dom2 c) = let newDom = fixDomain dom1 b dom2
                                  in Rf newDom (c . b)
