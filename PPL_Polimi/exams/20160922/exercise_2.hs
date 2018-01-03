module Exercise2(transpose, main) where

-- Given a list of lists, define a function transpose that returns a
-- list containing:
--   a list of all the first elements,
--   then a list of all the second elements, and so on.
-- Lists can be assumed non empty, but can be of different
-- lengths. Write all the types of the defined functions.

transpose :: [[a]] -> [[a]]
transpose lol
    | null valid = []
    | otherwise = heads : transpose (map tail valid)
    where
        valid = filter (not . null) lol
        heads = map head valid

-- E.g. transpose [[1,2],[3],[4,5,6]]
-- is the list [[1,3,4],[2,5],[6]].

main :: IO()
main = do
    print "transpose [[1,2],[3],[4,5,6]] ="
    print $ transpose [[1,2],[3],[4,5,6]]
