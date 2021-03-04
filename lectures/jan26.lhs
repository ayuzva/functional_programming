\begin{code}
module Jan26 where

type RegExp = String -> Bool 

eps :: RegExp
eps = (== "")

char :: Char -> RegExp
char c = \s -> s == [c]

(|||) :: RegExp -> RegExp -> RegExp
e1 ||| e2 = \s -> e1 s || e2 s

-- Takes a list, and returns a list of all (prefix, suffix) pairs
split, split', split'' :: [a] -> [ ([a], [a]) ]
split [] = []
split (y : ys) = ([], y : ys) ++ [ (y : ps, qs) | (ps, qs) <- split ys]

split' s = map ('splitAt' s)  

(<*>), (<**>) :: RegExp -> RegExp -> RegExp
e1 <*> e2 = \s -> or [e1 x && e2 y | (x, y) <- split' s] 
e1 <**> e2 = \s -> or [e1 x && e2 y | (x, y) <- split'' s]

lang1 :: RegExp
lang1 = (char 'a' ||| char 'b') <**> 


\end{code}