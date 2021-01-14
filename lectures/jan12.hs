This file is now a Latex file with embedded Haskell
\begin{code}
--Example code for jan12, to get people reacquainted with Haskell
module Jan12 where
    --list stuff
    map' :: (a -> b) -> [a] -> [b]
    map' f []       = []
    map' f (x : xs) = f x : map' f xs

    filter' :: (a -> Bool) -> [a] -> [a]
    filter' p []        = []
    filter' p (x : xs)  | p x       = x : filter' p xs
                        | otherwise = filter' p xs

    isEven :: Integer -> Bool
    isEven x = x `mod` 2 == 0

    --list comprehension:
    --[ x | x <- l, isEven x]

    l :: [Integer]
    l = [-15..15]

    --basic hello world
    main :: IO ()
    main = print "Hello world!"
\end{code}