module Exercise1(checkLt, checklist,
                 main) where

-- 1) Define a data structure, called Lt, for generic list of lists,
--    where each list has a fixed length and such number is stored in
--    the data structure.

data Lt a = Lt Int [[a]] deriving (Show)

-- 2) Define a function, called checkLt, that takes an Lt and returns
--    true if it is valid (i.e. every list in it have the correct size),
--    false otherwise.

checkLt :: Lt a -> Bool
checkLt (Lt _ []) = True
checkLt (Lt len (l:ls)) = (length l == len) && checkLt (Lt len ls)

-- 3) Define a function, called checklist, that takes a list t and an
--    Lt, checks if all the sublists of t are in the given Lt,
--    and uses Maybe to return the list of sublists of t that are not
--    present in Lt.
--    Note: sublists must be contiguous, e.g. the sublists of size 2 of
--    [1,2,3] are [1,2], [2,3].

sublistOfSize :: Int -> [a] -> [[a]]
sublistOfSize _ [] = []
sublistOfSize n (x:xs) = if n < length (x:xs) then
                            take n (x:xs) : sublistOfSize n xs
                         else [take n (x:xs)]

sublists :: [a] -> [[a]]
sublists xs = concat [sublistOfSize n xs | n <- [1..length xs]]

-- Returns [] if l is found in lt. It returns l otherwise.
inLt :: (Eq a) => Lt a -> [a] -> [a]
inLt (Lt _ []) l = l
inLt (Lt len (x:xs)) l = if l == x then []
                         else inLt (Lt len xs) l

checklist :: (Eq a) => [a] -> Lt a -> Maybe [[a]]
checklist l lt = let subs = sublists l
                     missing = filter (/= []) $ map (inLt lt) subs
                 in Just missing

-- 1.4) Make Lt an instance of Functor.

instance Functor Lt where
    fmap f (Lt len lists) = Lt len [map f list | list <- lists]

-- Note: state all the types of the defined functions.

main :: IO()
main = do
    let x = [1, 2, 3]
    let lt = Lt 2 [[1, 2], [4, 5], [6, 7]]
    print $ checklist x lt

    -- Test instance of Functor
    print $ (+ 1) <$> lt