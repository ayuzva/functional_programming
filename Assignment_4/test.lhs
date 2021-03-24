\begin{code}

module Test where

--class Functor f where
--  fmap :: (a -> b) -> f a -> f b

--class Monad m where
--  -- >>= is pronounced 'bind'
--  (>>=) :: m a -> (a -> m b) -> m b
--  return :: a -> m a

data Tree a = Tip a | Branch (Tree a) (Tree a)

instance Functor Tree where
    fmap f (Tip x) = Tip (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
    pure x = Tip x
    (<*>) (Tip x) (Tip y) = Tip (x y) 
    --(<*>) (Tip x) (Tip y) = Tip (x y) 

instance Monad Tree where
    return x = Tip x
    (>>=) (Tip x) y = y x
    (>>=) (Branch l r) f = Branch ((>>=) l f ) ((>>=) r f)

--do x <- m
--    f x
--
-- is same as m >>= f

test = do x <- Nothing
          y <- Just 3
          return y

ex1 :: [[Integer]]
ex1 = do x <- [1, 2, 3]
         y <- [5, 6, 7]
         return [x, y]




\end{code}