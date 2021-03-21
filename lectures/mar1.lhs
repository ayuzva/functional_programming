\begin{code}
module Mar01 where
import Prelude hiding (Functor, Monad, fmap, return, (>>=))

class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

--List Monad:
instance Monad [] where
    return a = [a] -- sometimes called 'singleton'
{-  -- m has type n
    []  >>= f = []
    (x:xs) >>= f = --concat (f x : map f xs) -- what about the xs ?, map over them
                   --(f x) ++ (xs >>= f) -- alternative, but recursive defn -}
    l >>= f = concat $ map f l  -- a better version of the previous

ex1 :: [[Integer]]
ex1 = do x <- [1, 2, 3]
         y <- [5, 6, 7]
         return [x,y]

\end{code}

Concat is used to remove another level of list, because a function can return multiple returns

- Tree monad (easiest: tree with data at the leaves only, Also want to use something like concat
but for trees)

- State monad, is a like a heap (memory)
    - will record some information and move on to the next state

weird ones:
-Backward State monad 
    - minimalistic backtracking
    - used in symbolic assemblers
    - you can send information back to previous states

-Tardis monad
    - combines forward and backward state, recursively
    - can both send information to the futre and the past
    - will make sure when the information is sent both way and that time lines will make sense

- Continuation monad (Important)
    - on values of type 'a' and return type 'r'
    - control structure that captures the future of a computation
\begin{code}
newtype Cont r a = Cont { runCont :: (a -> r) -> r}

\end{code}
Biggest example: try-catch
    -in the try block you have a future written down as a program, it is given current stat of the
    program, but if something goes wrong, it backtracks and tries and alternative future. 

Monad Laws
- laws in term of >>=, even in terms of 'do' look ugly, hard to understand.
Auxiliary Function to simply it
\begin{code}
(>@>) :: Monad m => (a -> m b) -> 
                    (b -> m c) ->
                    (a -> m c)
f >@> g = \x -> f x >>= g
\end{code}

Define f =o= g as forall x, f x == g x
Laws:
1. left - unit law  -- Singleton computations don't behave any differently than applying a function
    return >@> f =o= f

2. right - unit law
    f >@> return =o= f

-- it's like 0 + x = x or 1*y = y or "" ++ z = z for the left-unit laws

3. Associativity laws
    (f >@> g) >@> h == f >@> (g >@> h)

You implicitly assume that sequencing is associative all the time ...