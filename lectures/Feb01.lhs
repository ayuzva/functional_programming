\begin{code}

{-# OPTIONS_GHC -wall #-}
module Feb01 where

\end{code}

\begin{code}
data Tree a = Nil | Node a (Tree a) (Tree a)

collapse :: Tree a -> [ a ]
collapse Nil = []
collapse (Node x l r) = (x : collapse l) ++ collapse r

ex1 :: Tree Integer
ex1 Node 6 (Node 5 Nil Nil) (Node 3 Nil (Node 17 Nil Nil))
\end{code}
 
