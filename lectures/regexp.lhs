type RegExp = String -> Bool

eps :: RegExp
eps s = s == ""

char :: Char -> RegExp
char c  = \s -? s == [c]

(|||) :: RegExp -> RegExp -> RegExp
e1 ||| e2 = \s -> e1 s || e2 s

Sequencing
\begin{code}
split, split', split'' :: [a] -> [([a], [a])]
split [] = []
split (y : ys) = ([], y : ys) ++ [ (y : ps, qs) | (ps, qs) <- split ys ]

split' s = map ('splitAt' s) [0..length s] 
split'' s = map ('splitAt' s) [1..length s] 

(<*>). (<**>) :: RegExp -> RegExp -> RegExp
e1 <*> = \s -> or [e1 x && e2 y | (x, y) <- split' s] 
e1 <**> = \s -> or [e1 x && e2 y | (x, y) <- split'' s]

star :: RegExp -> RegExp
star e = eps ||| (e <**> star e)

lang1 :: RegExp
lang1 = (char 'a' ||| char 'b') <**> star (char 'c')

lang2 :: RegExp
lang2 = star ((eps ||| char 'd') <*> char 'x')

lang3 :: RegExp
lang3 = star ((eps ||| char 'd') <*> char 'c') 
\end{code}

