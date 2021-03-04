\begin{code}

module Jan25 where

Concept: "extensionality", answers the question "when are two functions equal?"
(forall x. f x = g x) -> f = g
opposite idea: "intensionality". Roughly: syntax matters.

Typecalsses: mecahnism for overloading
reusaility of \textbf{concepts}

Ex: |Eq| class - express that you \emph{may} have an equality.
contrast Java which has an equals method in Java.Lang.Object
counter-example: functions! in general, undecidable. 


\begin{code}
lookup :: Eq a => a -> [(a, b)] -> [b]
lookup _ [] = []
lookup x ((a, b) : ys) =
    if x == a then b : lookup x ys else lookup x ys

lookup' :: Eq a => a -> [(a, b)] -> [b]
lookup' x  = filter (== x)
\end{code}
Note: the type | [(a, b)] | is known as 'association lists'

\newpage
\begin{code}
type RegExp = String -> Bool

eps:: RegExp
--eps s = s == "" This can be condensed to:
eps = (== "")

char :: Char -> RegExp
char c = \s -> s == [c]

(|||) :: RegExp -> RegExp -> RegExp
e1 ||| e2 = \s -> e1 s || e2 s

\end{code}
Q: recursion in type synonyms
A: no -- try |type Foo = Bool -> Foo|

