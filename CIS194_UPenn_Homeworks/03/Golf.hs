module Golf(skips, localMaxima, histogram,
            main, skipsEx, localMaximaEx, histrogramEx) where

-- Exercise 1 Hopscotch

-- Your first task is to write a function
--      skips :: [a] -> [[a]]
-- The output of skips is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should
-- contain every second element from the input list. . . and the nth list in
-- the output should contain every nth element from the input list.
-- For example:
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
-- Note that the output should be the same length as the input.

every :: Int -> [a] -> [a]
every n xs = case drop (n - 1) xs of
                (y:ys) -> [y] ++ every n ys
                [] -> []

skips :: [a] -> [[a]]
skips l = skips' l 1 where
    skips' l step
        | step == (length l) + 1 = []
        | otherwise = every step l : skips' l (step + 1)

-- Exercise 2 Local maxima
-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it. For
-- example, in the list [2,3,4,1,5], the only local maximum is 4, since
-- it is greater than the elements immediately before and after it (3 and
-- 1). 5 is not a local maximum since there is no element that comes
-- after it.
-- Write a function
--      localMaxima :: [Integer] -> [Integer]
-- which finds all the local maxima in the input list and returns them in
-- order. For example:
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
    | x < y && y > z = y : localMaxima (y:z:xs)
    | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

-- Exercise 3 Histogram

-- For this task, write a function
--      histogram :: [Integer] -> String
-- which takes as input a list of Integers between 0 and 9 (inclusive),
-- and outputs a vertical histogram showing how many of each number
-- were in the input list. You may assume that the input list does not
-- contain any numbers less than zero or greater than 9 (that is, it does
-- not matter what your function does if the input does contain such
-- numbers). Your output must exactly match the output shown in the
-- examples below.
-- histogram [1,1,1,5] ==
--  *
--  *
--  * *
-- ==========
-- 0123456789
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
--  *
--  *
--  * *
--  ****** *
-- ==========
-- 0123456789
-- Important note: If you type something like histogram [3,5] at
-- the ghci prompt, you should see something like this:
-- " * * \n==========\n0123456789\n"
-- This is a textual representation of the String output, including \n
-- escape sequences to indicate newline characters. To actually visualize
-- the histogram as in the examples above, use putStr, for example,
-- putStr (histogram [3,5]).

incrementNth :: Int -> [Integer] -> [Integer]
incrementNth n l = let (prefix, suffix) = splitAt n l
                       nth = head suffix
                   in prefix ++ [nth + 1] ++ tail suffix

frequencies :: [Integer] -> [Integer]
frequencies xs = helper xs [0 | _ <- [0..9]] where
    helper [] acc = acc
    helper (y:ys) acc = helper ys (incrementNth (fromIntegral y) acc)

histogram :: [Integer] -> String
histogram xs = helper freqs (maximum freqs) where
    freqs = frequencies xs
    helper :: [Integer] -> Integer -> String
    helper ys 0 = "==========\n0123456789\n"
    helper ys cur = concatMap (\v -> if v == cur then "*" else " ") ys ++
                    "\n" ++
                    helper 
                        (map (\v -> if v == cur then v-1 else v) ys)
                        (cur - 1)

skipsEx :: IO()
skipsEx = do {
    putStrLn $ "skips \"ABCD\" = " ++ show (skips "ABCD");
    putStrLn $ "skips \"Hello!\" = " ++ show (skips "Hello!");
}

localMaximaEx :: IO()
localMaximaEx = do {
    putStrLn $ "localMaxima [2,9,5,6,1] = " ++ show (localMaxima [2,9,5,6,1]);
    putStrLn $ "localMaxima [2,3,4,1,5] = " ++ show (localMaxima [2,3,4,1,5]);
}

histrogramEx :: IO()
histrogramEx = do {
    putStrLn $ "histogram [1,4,5,4,6,6,3,4,2,4,9] = \n" ++ histogram [1,4,5,4,6,6,3,4,2,4,9];
    putStrLn $ "histogram [1,1,1,5] = \n" ++ histogram [1,1,1,5];
}

main :: IO()
main = do {
    putStrLn "Usage: [skipsEx | localMaximaEx | histrogramEx]";
}