module Exercise2() where

-- Define a data structure for Clists with a data declaration.
-- Make Clist an instance of Eq – beware: equality test must always terminate

data Clist a = Clist a (Clist a)
             | End (Clist a)

instance (Eq a) => Eq (Clist a) where
    (End _) == (End _) = True
    (Clist l nextl) == (Clist r nextr) = l == r && nextl == nextr
    _ == _ = False

instance (Show a) => Show (Clist a) where
    show l = "[" ++ helper l ++ "]" where
        helper (End _) = "End"
        helper (Clist v next) = show v ++ ", " ++ helper next

-- Define two functions list2clist and clist2list, that are used to convert
-- an ordinary list to a Clist, and vice versa.
-- Write their types.

list2clist :: [a] -> Clist a
list2clist (x:xs) = let first = Clist x $ list2clist' xs first
                    in first

list2clist' :: [a] -> Clist a -> Clist a
list2clist' [] first = End first
list2clist' (x:xs) first = Clist x $ list2clist' xs first

clist2list :: Clist a -> [a]
clist2list (End _) = []
clist2list (Clist v next) = v : clist2list next

-- Define cmap, a map operation for Clists. Write its type.

cmap :: (a -> b) -> Clist a -> Clist b
cmap f clist = let l = clist2list clist
                   l' = map f l
               in list2clist l'