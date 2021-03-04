\begin{itemize}
\item fold
\item lazyness
\end{itemize}

\begin{code}
fold_left :: (b -> a -> b) -> b -> [a] -> b
fold_left f s [] = s
fold_left f s (x : xs) = fold_left f (f s x) xs
\end{code}

Later on: "recursion schemes"
In functional languages, functions are first-class values.

Here is some very odd code:

Zip takes two lists, applies the function until one of the lists runs out.

Take, takes a number of elements of the list from the begining. 
\begin{code}
l :: [Integer]
l = 1 : 1 : zipWith (+) l (tail l)
\end{code}


