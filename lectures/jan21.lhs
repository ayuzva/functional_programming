\begin{code}

module Jan21 where
import Prelude hiding (curry, uncurry)
\end{code}

\begin{itemize}
\item higher-order functions
\item typeclasses
\item extensionality
\end{itemize}

$ is basically apply with very low precedence
\begin{code}
weirdo = zipWith ($)
\end{code}

\begin{code}
mapFun :: [a -> b] -> a -> [b]
-- v3
mapFun f x = map ($ x) f
-- v2
--mapFun f x = map (\g -> g $ x) f
-- v1
--mapFun [] _ = []
--mapFun (f : fs) x = f x : mapFun fs x
\end{code}

\begin{code}
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \a b -> f (a, b)
--also x
--curry f a b = f (a, b)

uncury :: (a -> b -> c) -> ((a, b) -> c)
uncury f = \(a, b) -> f a b
--uncury f (x, y) = f x y
\end{code}