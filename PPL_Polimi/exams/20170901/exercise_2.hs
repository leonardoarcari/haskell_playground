module Exercise2(constItree, list2Itree,
                 takeLevels,
                 applyAtLevel) where
-- 1) Define an infinite binary tree data structure, called Itree,
--    i.e. a binary tree containing an infinite number of nodes, each
--    containing a value, and in which every path from the root downwards
--    is infinite.

data Itree a = INode (Itree a) a (Itree a)

-- 2) Is it possible to automatically derive Show for Itree? 
--    If yes, do it; if not, state why and write a reasonable 
--    implementation of show for Itrees.

-- It's not possible because show would never return since it will try to
-- recursively call itself on each subtree, never reaching the end of
-- the path
instance (Show a) => Show (Itree a) where
    show (INode _ a _) = "< ... | " ++ show a ++ " | ... >"

-- 3) Define costItree which takes a value x and returns an Itree
--    where all the contained values are equal to x.

constItree :: a -> Itree a
constItree x = INode (constItree x) x (constItree x)

-- 4) Define list2Itree, which takes an infinite list L
--    and returns an Itree T in which every path from the root downwards
--    contains the same sequence of values of those in L (of course,
--    all values in L but the first are duplicated in T).

list2Itree :: [a] -> Itree a
list2Itree [] = error "List is not infinite!"
list2Itree (x:xs) = INode (list2Itree xs) x (list2Itree xs)

-- 5) Make Itree an instance of Functor.

instance Functor Itree where
    fmap f (INode l v r) = INode (f <$> l) (f v) (f <$> r)

-- 6) Define a function takeLevels which takes a natural number n
--    and an Itree T and returns a (finite) binary tree that represents
--    the top subtree of T from its root to level n.
--    (Note that the root has level 0, its sons level 1, and so on.)

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show)

takeLevels :: Int -> Itree a -> Tree a
takeLevels (-1) _ = Leaf
takeLevels n (INode l v r) = Node (takeLevels (n-1) l) v (takeLevels (n-1) r)

-- 7) Define a function applyAtLevel which takes a function f, a predicate
--    p on levels (i.e. a function Int → Bool), and an Itree T, and
--    returns an Itree that is identical to T, but for levels that satisfy
--    p: those are updated by applying f to their values.
--    E.g. applyAtLevel (+1) odd $ constItree 1 is an Itree where the root
--         has value 1, its sons 2, their sons 1, and so on, alternatively.
--    Note: you are required to write all the types of the functions you
--    define.

applyAtLevel :: (a -> a) -> (Int -> Bool) -> Itree a -> Itree a
applyAtLevel f' p' t = helper f' p' t 0 where
    helper f p (INode l v r) level = if p level then
                                        INode (helper f p l (level + 1)) (f v) (helper f p r (level + 1))
                                     else
                                        INode (helper f p l (level + 1)) v (helper f p r (level + 1))