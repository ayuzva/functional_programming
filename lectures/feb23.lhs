<<<<<<< HEAD
\begin{code}
module Feb23 where
import qualified Prelude as P
import Prelude hiding (Functor)
\end{code}

Unification: Given two types s, u find (if possible) a set of substitutions s, s such that

    subst s t == subst s u

where == means syntactically equal, I.E. identical

ex: t = [ Char ], u = [ a ]
then s = [ a | -> Char ] is such a solution

    [a |-> Char] t = [Char]
    [a |-> Char] u = [Char]

ex: t = Either a (b -> c)  u = Either b (b -> d)

    s = [a |-> b, c |-> d]

    s @ t = Either b (b -> d) s @ u = Either b (b -> d)
    
    Alternatively:

    s = [b |-> a, c |-> d]

    s @ t = Either a (a -> d)  s @ u = Either a (a -> d)


Unifications algorithm, in pseudocode:

unify :: pattern -> pattern -> Maybe Unifier

Unifier = list of substitutions
substitutions = variable |-> type

ex: t = Char u = Bool -- Fail, Nothing you can do to make those equal
ex: t = Either a b     u = [c] -- Again, fail, they are much different types

valid patterns:
1. constant names of types, like Char, Bool
2. variables like a, b, c
3. constructors, like Either a b, [c], Maybe a, ...
    all constuctors have a fixed 'arity', i. e. number of arguments.

If you try to put *functions* at the type level, things go weird fast...
- second order unification is undecidable

unify :: pattern -> pattern -> Maybe Unifier
unify x  x = Just [] -- where x is a constant type name
unify x  y = Nothing -- where x, y are a constant type names
unify x  v = Just [ v |-> x] --v varialbe, x type constant name
unify v  x = Just [ v |-> x] --ditto --same thing again
unify v  (C t1 t2 .. tn) = Just [v |-> C t1 t2 .. tn] --In some sense 4 is kinda like this one

--and symmetric case
unify (C t1 t2 ... tn) (D u1 ... um)
    | C == D && n == m = unify t1 u1, unify t2, u2, ... unify tn un
    | otherwise = Nothing
        --also need to check if solution is coherent

ex (where things go wrong)
t = a     u = a -> a

"occurs check"

-----------------------------------------------------------------------------------
\begin{code}

--DATA is similar to newtype, but used
--when constructors has more than one field inside it


--NEWTYPE is similar to data, but only allows constuctors with one
--field in it

newtype Fd = Fd CInt
-- data Fd = Fd CInt would also be valid

-- newtypes can have deriving clauses just like normal types
newtype Identity a = Identity a
  deriving (Eq, Ord, Read, Show)

-- record syntax is still allowed, but only for one field
newtype State s a = State { runState :: s -> (s, a) }

newtype NPair a b = NPair (a, b)

--CLASS is a type class, instance belong to the type class
-- a way to group similar types

--INSTANCE is instance of a type class


--let coupled with in "let ... in ..." is an expression
--it is allowed anywhere unlike "where"

--where -- is bound to a surrounding construct, like pattern
--matching line of a function definition

-- ======================Class Notes==================================================

class Functor f where --type class 
    fmap :: (a -> b) -> f a -> f b

--for f a to well typed, f must have kind * -> *, i. e. be a 
--    /type constructor/

--A type constructor 'f' is satisfies 'Functor', roughly when
--1. f is sort of container
--2. f is a Just in Time container, i.e. something that will contain 'a's when needed
instance Functor [] where -- individual class that belongs to class functor
    fmap f l = map f l

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)

--thus fmap :: (c -> d) -> Either b c -> Either b d
instance Functor (Either b) where -- sliced of last var to make type constructor
    fmap f (Left b) = Left b
    fmap f (Right c) = Right (f c)

newtype State s a = State (s -> (s, a))

instance Functor (State s) where
    fmap f (State g) = State $ \s -> 
        let (s', a) = g s in
        (s' , f a)  

\end{code}
Laws:
1. for all x. fmap id x = id x 
2. for all x, f, g. fmap f (fmap g x) = fmap (f . g) x

Prove Functor for Maybe satisifes the laws:

1. first two casses:
    fmap id Nothing == fmap. Maybe.1
    Nothing         == id.1
    id Nothing     


    fmap id (Just x) == fmap. Maybe .2
    Just (f x)       == id.1
    id (Just (f x))

2. second laws
    fmap f (fmap g Nothing) == fmap. Maybe 1
    fmap f Nothing          == fmap Maybe 1
    Nothing                 == fmap Maybe 1
    fmap (f. g) Nothing

    fmap f (fmpa g Just x) == fmap. maybe.2
    fmap f (Just (g x))    == fmap. maybe.2
    Just (f (g x))         == (.) defn
    Just ((f. g) x)        == fmap.maybe 2
    fmap (f.g) (Just x)

Reader: "Values that depend o an environment"

\begin{code}
newtype Reader e a = Reader (e -> a)

instance Functor (Reader e) where 
-- f :: a -> b
-- r :: e -> a
    fmap f (Reader r) = Reader $ \e -> f (r e)
    -- if you want to go point-free:
    fmap f (Reader r) = Reader (f . r)
-- Reader functor works by pre-composition, f is applied to r
--making e -> b

\end{code}
================================================================
Monad
Roughly: (representation of) computations that sequence
Think of them as not being computations, but representing computations
Forces to do things in order
They compose

    sequence == ordered composition 

    note the ';' of C, Java is forward order composition, aka sequencing
    Java's ';' is basically (flip (.)) in word-passing style
    world  ~~~ the heap

\begin{code}
class Monad m where
    -- >>= is pronounced 'bind'
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
\end{code}

-- Back to Modelling computations that can fail ...
-- Simplest case: result is either a value, or nothing, i.e. Maybe

\begin{code}
instance Monad Maybe where
    -- m :: Maybe a
    -- f :: (a -> Maybe b)
    Nothing  >>= _ = Nothing
    (Just x) >>= f = f x
    return x       = Just x 
\end{code}

Examples: 
\begin{code}
let z = 
    Just 5 >>= (\x -> return $ x+2)
        >> = (\x -> return $ x * 7)

\end{code}

Sugar time!
do x <- m --bind an x to the result of computation m
    f x  --continue computation with f

is the same as
m >>= f

z' :: Maybe Integer
z' = do x <- Just 5
        y <- return $ x + 2
        z <- return $ y * 7
        return z

w' :: Maybe Integer
w' = do x <- Just 5 --experiment change to Nothing
        y <- Just 3
        return $ x + y

Enables writing:
w'' = do       --This looks like imperative code
    x <- f n
    y <- g n
    t <- h y
    return (t*(x+y))

There is no try catch, there is no if's. Fails nicely

Monads: 
--Maybe (potentially failing computation)
--Identity monda (pure values)
--List (kind of represents non-deterministic computations)
--Reader (computations in an environment)
--State (stateful computations), i. e. heap, world-passing, ...

.. and then it gets weird
-- Cont (Continuation monad)
-- Tardis 
=======
\begin{code}
module Feb23 where
import qualified Prelude as P
import Prelude hiding (Functor, Monad, fmap, return)
\end{code}

Unification: Given two types s, u find (if possible) a set of substitutions s, s such that

    subst s t == subst s u

where == means syntactically equal, I.E. identical

ex: t = [ Char ], u = [ a ]
then s = [ a | -> Char ] is such a solution

    [a |-> Char] t = [Char]
    [a |-> Char] u = [Char]

ex: t = Either a (b -> c)  u = Either b (b -> d)

    s = [a |-> b, c |-> d]

    s @ t = Either b (b -> d) s @ u = Either b (b -> d)
    
    Alternatively:

    s = [b |-> a, c |-> d]

    s @ t = Either a (a -> d)  s @ u = Either a (a -> d)


Unifications algorithm, in pseudocode:

unify :: pattern -> pattern -> Maybe Unifier

Unifier = list of substitutions
substitutions = variable |-> type

ex: t = Char u = Bool -- Fail, Nothing you can do to make those equal
ex: t = Either a b     u = [c] -- Again, fail, they are much different types

valid patterns:
1. constant names of types, like Char, Bool
2. variables like a, b, c
3. constructors, like Either a b, [c], Maybe a, ...
    all constuctors have a fixed 'arity', i. e. number of arguments.

If you try to put *functions* at the type level, things go weird fast...
- second order unification is undecidable

unify :: pattern -> pattern -> Maybe Unifier
unify x  x = Just [] -- where x is a constant type name
unify x  y = Nothing -- where x, y are a constant type names
unify x  v = Just [ v |-> x] --v varialbe, x type constant name
unify v  x = Just [ v |-> x] --ditto --same thing again
unify v  (C t1 t2 .. tn) = Just [v |-> C t1 t2 .. tn] --In some sense 4 is kinda like this one

--and symmetric case
unify (C t1 t2 ... tn) (D u1 ... um)
    | C == D && n == m = unify t1 u1, unify t2, u2, ... unify tn un
    | otherwise = Nothing
        --also need to check if solution is coherent

ex (where things go wrong)
t = a     u = a -> a

"occurs check"

-----------------------------------------------------------------------------------
\begin{code}

--DATA is similar to newtype, but used
--when constructors has more than one field inside it

--TYPE  is a just allias for existing data type type Identifier = String

--NEWTYPE is similar to data, but only allows constuctors with one
--field in it

--CLASS is a TYPE CLASS!!!!, instance belong to the type class
-- a way to group similar types

--INSTANCE is instance of a type class

newtype Fd = Fd Int
-- data Fd = Fd Int would also be valid

-- newtypes can have deriving clauses just like normal types
newtype Identity a = Identity a
  deriving (Eq, Ord, Read, Show)

-- record syntax is still allowed, but only for one field
newtype State s a = State { runState :: s -> (s, a) }

newtype NPair a b = NPair (a, b)

--let coupled with in "let ... in ..." is an expression
--it is allowed anywhere unlike "where"

--where -- is bound to a surrounding construct, like pattern
--matching line of a function definition

-- ======================Class Notes==================================================

class Functor f where --type class 
    fmap :: (a -> b) -> f a -> f b

--for f a to well typed, f must have kind * -> *, i. e. be a 
--    /type constructor/

--A type constructor 'f' is satisfies 'Functor', roughly when
--1. f is sort of container
--2. f is a Just in Time container, i.e. something that will contain 'a's when needed
instance Functor [] where -- individual class that belongs to class functor
    fmap f l = map f l

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)

--thus fmap :: (c -> d) -> Either b c -> Either b d
instance Functor (Either b) where -- sliced of last var to make type constructor
    fmap f (Left b) = Left b
    fmap f (Right c) = Right (f c)

instance Functor (State s) where
    fmap f (State g) = State $ \s -> 
        let (s', a) = g s in
        (s' , f a)  

\end{code}
Laws:
1. for all x. fmap id x = id x 
2. for all x, f, g. fmap f (fmap g x) = fmap (f . g) x

Prove Functor for Maybe satisifes the laws:

1. first two casses:
    fmap id Nothing == fmap. Maybe.1
    Nothing         == id.1
    id Nothing     


    fmap id (Just x) == fmap. Maybe .2
    Just (f x)       == id.1
    id (Just (f x))

2. second laws
    fmap f (fmap g Nothing) == fmap. Maybe 1
    fmap f Nothing          == fmap Maybe 1
    Nothing                 == fmap Maybe 1
    fmap (f. g) Nothing

    fmap f (fmpa g Just x) == fmap. maybe.2
    fmap f (Just (g x))    == fmap. maybe.2
    Just (f (g x))         == (.) defn
    Just ((f. g) x)        == fmap.maybe 2
    fmap (f.g) (Just x)

Reader: "Values that depend o an environment"

\begin{code}
newtype Reader e a = Reader (e -> a)

instance Functor (Reader e) where 
-- f :: a -> b
-- r :: e -> a
    fmap f (Reader r) = Reader $ \e -> f (r e)
    -- if you want to go point-free:
    --fmap f (Reader r) = Reader (f . r)
-- Reader functor works by pre-composition, f is applied to r
--making e -> b

\end{code}
================================================================
Monad
Roughly: (representation of) computations that sequence
Think of them as not being computations, but representing computations
Forces to do things in order
They compose

    sequence == ordered composition 

    note the ';' of C, Java is forward order composition, aka sequencing
    Java's ';' is basically (flip (.)) in word-passing style
    world  ~~~ the heap

\begin{code}
class Monad m where
    -- >>= is pronounced 'bind'
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
\end{code}

-- Back to Modelling computations that can fail ...
-- Simplest case: result is either a value, or nothing, i.e. Maybe

\begin{code}
instance Monad Maybe where
    -- m :: Maybe a
    -- f :: (a -> Maybe b)
    Nothing  >>= _ = Nothing
    (Just x) >>= f = f x
    return x       = Just x 
\end{code}

Examples: 
\begin{verbatim}
let z = Just 5 >>= (\x -> return $ x+2)
               >>= (\x -> return $ x * 7)

Sugar time!
do x <- m --bind an x to the result of computation m
    f x  --continue computation with f

is the same as
m >>= f

z' :: Maybe Integer
z' = do x <- Just 5
        y <- return $ x + 2
        z <- return $ y * 7
        return z
\end{verbatim}
\begin{code}
w' :: Maybe Integer
w' = do x <- Nothing --experiment change to Nothing
        y <- Just 3
        return $ x + y
\end{code}
\begin{verbatim}
Enables writing:
w'' = do       --This looks like imperative code
    x <- f n
    y <- g n
    t <- h y
    return (t*(x+y))

There is no try catch, there is no if's. Fails nicely

Monads: 
--Maybe (potentially failing computation)
--Identity monad (pure values)
--List (kind of represents non-deterministic computations)
--Reader (computations in an environment)
--State (stateful computations), i. e. heap, world-passing, ...

.. and then it gets weird
-- Cont (Continuation monad)
-- Tardis 
\end{verbatim}

\begin{code}
-- main = do line <- fmap reverse getLine  
--           putStrLn $ "You said " ++ line ++ " backwards!"  
--           putStrLn $ "Yes, you really said" ++ line ++ " backwards!"  

--Functor is for maping over types
--Monad is for chaning type computations
    --note that do is equivalent to >>= 
    --Also note that chained computations need not be sequential
    --They can get calculated in paralel

z'' :: Maybe Integer
z'' = do x <- Just 5
         y <- return $ x + 2
         z <- return $ Nothing
         return y

\end{code}

>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
