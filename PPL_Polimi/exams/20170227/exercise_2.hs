module Exercise2(lol2lolstream,
                 getPeriodicity, getStream,
                 main) where

-- Consider this data declaration:
data LolStream x = LolStream Int [x]

getPeriodicity :: LolStream x -> Int
getPeriodicity (LolStream p _) = p

getStream :: LolStream x -> [x]
getStream (LolStream _ stream) = stream

-- The list [x] must always be an infinite list (also called a stream),
-- while the first parameter, of type Int, when positive represents the
-- fact that the stream is periodic, while it is not periodic if
-- negative (0 is left unspecified).
-- E.g. these are two valid LolStreams:
--      LolStream -1 [1,2,3..]
--      LolStream 2 [1,2,1,2,1,2..]

-- 1) Define a function lol2lolstream which takes a finite list of finite
--    lists [h1, h2, … hn], and returns
--      LolStream (|h1| + |h2| + … + |hn|) (h1 ++ h2 ++ … ++ hn ++ h1 ++ h2 ++ …)

lol2lolstream :: [[a]] -> LolStream a
lol2lolstream lol = let periodicity = sum (length <$> lol)
                        ilol = concat $ repeat (concat lol)
                    in LolStream periodicity ilol

-- 2) Make LolStream an instance of Eq. (Note: == should terminate,
--    when possible.)

instance (Eq a) => Eq (LolStream a) where
    (LolStream p1 xs) == (LolStream p2 ys) =
        take maxPer xs == take maxPer ys
            where maxPer = max p1 p2

-- 3) Make LolStream an instance of Functor, Foldable, and Applicative.

instance Functor LolStream where
    fmap f (LolStream p stream) = LolStream p (f <$> stream)

instance Foldable LolStream where
    foldr f start (LolStream _ stream) = foldr f start stream

instance Applicative LolStream where
    pure a = LolStream 1 (repeat a)
    (LolStream _ fs1) <*> (LolStream p2 s2)
        = LolStream p2 stream' where
            stream' = concatMap (\f -> f <$> s2) fs1

-- Bonus (+3 pts) Make LolStream an instance of Monad

instance Monad LolStream where
    return = pure
    (LolStream p s) >>= f = let lolStreams = map f s
                                stream = concatMap getStream lolStreams
                            in LolStream p stream

-- Driver

streamComprehension :: LolStream a -> LolStream b -> LolStream (a, b)
streamComprehension s1 s2 = do x <- s1
                               y <- s2
                               return (x, y)

main :: IO()
main = do
    let h1 = [1, 2, 3]
    let h2 = [4, 5]
    let lolStream = lol2lolstream [h1, h2]
    print $ take 12 $ getStream lolStream

    -- Test Eq instance
    print $ lolStream == lol2lolstream [h1]
    print $ lolStream == lol2lolstream [h1, h2]
    print $ lol2lolstream [[1,2]] == lol2lolstream [[1, 2], [1, 2]]

    -- Test Applicative instance
    let fStream = lol2lolstream [[(+1)], [(*2)]]
    print $ take 10 $ getStream $ fStream <*> lolStream

    -- Test Monad instance
    let compr = streamComprehension (lol2lolstream [h1]) (lol2lolstream [h2])
    print $ take 10 $ getStream $ compr