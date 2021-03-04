\begin{code}

{-# OPTIONS_GHC -wall #-}
module Feb01 where

\end{code}

\begin{code}
data Tree a = Nil | Node a (Tree a) (Tree a)
    deriving Show

collapse' :: Tree a -> [ a ]
collapse' Nil = []
collapse' (Node x l r) = (x : collapse l) ++ collapse r

ex1 :: Tree Integer
ex1 Node 6 (Node 5 Nil Nil) (Node 3 Nil (Node 17 Nil Nil))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Nil =  Nil
mapTree f (Node x l r) = Node (f x ) (mapTree f l) (mapTree f r)
\end{code}

\begin{code}
data Maybe a = Nothing | Just a

-- eliminator
maybe :: b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b 
maybe _ f (Just a) = f a 

data Either a b = Left a | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left a) = f a
either f g (Right b) = g b

\end{code}
