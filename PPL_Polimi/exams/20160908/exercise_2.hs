module Exercise2(checkFig, main) where

-- Consider the language of pictures L as in Exercise 1. Define the checkFig
-- function, which takes a list of lists p and returns Just n, where n is the
-- side of p, if p is a member of L; Nothing otherwise. Write all the types of
-- the defined functions.

data Lit = Zero | One deriving (Eq, Show)

checkRow :: [Lit] -> Int -> Int -> Bool
checkRow [] _ _ = True
checkRow (x:xs) cur pos
    | cur == pos && x == One = True && checkRow xs (cur+1) pos
    | cur /= pos && x == Zero = True && checkRow xs (cur+1) pos
    | otherwise = False

checkFig :: [[Lit]] -> Maybe Int
checkFig (x:xs) = helper (x:xs) 0 (length x) where
    helper :: [[Lit]] -> Int -> Int -> Maybe Int
    helper [] _ size = Just size
    helper (x:xs) pos size
        | length x == size = case checkRow x 0 pos of
                                True -> helper xs (pos+1) size
                                _ -> Nothing
        | otherwise = Nothing

main :: IO()
main = do
    print $ checkFig [[One, Zero, Zero], [Zero, One, Zero], [Zero, Zero, One]]
    print $ checkFig [[One, Zero, Zero], [One, One, Zero], [Zero, Zero, One]]
    print $ checkFig [[Zero, One, Zero], [One, Zero, Zero], [Zero, Zero, One]]