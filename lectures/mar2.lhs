\begin{code}
{-# OPTIONS_GHC -Wall #-}
module Mar02 where
\end{code}

Learning objectives:
Monad parsing

Will be focusing on practice rather than proofs

Is -> a monad, no but e -> is, i.e. Reader. ((->) a) b

\begin{code}
(>>) :: Monad m -> m a -> m b -> m b
m >> f = m >>= \ _ -> f

fail :: Monad m => String -> m a
fail s = error s

fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' f c = c >>= \x -> return $ f x
\end{code}

\begin{spec}
class P.Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
\end{spec}

<*> is kind of sequencing, where order doesn't matter.

Parsing! As a list of succesfull parses 
\begin{code}
type Parse a b = [a] -> (b, [a])
\end{code}

You won't be surprised that this a Monad....

First, some basic parsers:
\begin{code}
none :: Parse a b
none _ = []

succeed :: b -> Parse a b
succeed x inp = [(x, inp)]

token :: Eq a = a -> Parse a a
token _ [] = []
token a (x:xs)
    | a == x    = [(x, xs)]
    | otherwise = []

spot :: (a -> Bool) -> Parse a b
spot _ [] = []
spot p (x:xs)
    | p x = [(x, xs)]
    | otherwise = []

token' :: Eq a => a -> Parse a a
token' x = spot (== x)

newtype MP a b = MP { mp :: Parse a b}

--Functor applies a function to contents of the box to get output contents of the box
--Applicaitve applies a function in a box to contents in the box to get output contents in a box
instance Functor (MP c) where
    fmap f (MP p) = MP $ \s -> map (\(x, y) -> (x, map f y)) (p s)
  
instance Applicative (MP a) where


instance Monad (MP a) where


\end{code}

