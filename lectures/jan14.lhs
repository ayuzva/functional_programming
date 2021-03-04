\begin{code}
module Jan14 where
\end{code}

Learning objectives (planned):
\begin{itemize}
\item fold (generalze in later lecutres)
\end{itemize}

Folding right and left.
Context: a list, a binary function 'f'.

\lstinline1 = [a1, a2, a3, ..., a4] |
\begin{enumerate}
\item fold_right: a1 `f` (a2 `f` (a3 `f` ...(an `f` s)))
\item fold_left: (((s `f` a1) `f` a2) `f` a3) 
\end{enumerate}
\begin{code}

--foldr in Haskell
fold_right :: (a -> b -> b) -> b -> [a] -> b
fold_right f s []     = s
fold_right f s (x:xs) = x `f` (fold_right f s xs)

\end{code}

IMPORTANT: \textbf{The only thing you can do with a function is call it!}