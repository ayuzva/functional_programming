\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE KindSignatures #-}
module Mar11 where

class Symantics (rep :: * -> *) where
    int :: Integer -> rep Integer
    add :: rep Integer -> rep Integer -> rep Integer
    mul :: rep Integer -> rep Integer -> rep Integer 

    bool :: Bool -> rep Bool
    and_ :: rep Bool -> rep Bool -> rep Bool
    or_ :: rep Bool -> rep Bool -> rep Bool
    if_ :: rep Bool -> rep a -> rep a -> rep a

    leq :: rep Integer -> rep Integer -> rep Bool


newtype RR a = RR {unRR :: a}

liftRR2 :: (a -> b -> c) -> RR a -> RR b -> RR c
liftRR2 f = \x y -> RR $ f (unRR x) (unRR y)

liftRR3 :: (a -> b -> c -> d) -> RR a -> RR b -> RR c -> RR d
liftRR3 f = \x y z -> RR $ f (unRR x) (unRR y) (unRR z)

instance Symantics RR where
    int = RR 
    bool = RR

    --lift essentially avoids writting out wrap unwrap of types
    add = liftRR2 (+) -- (RR x) (RR y) = RR $ x + y
    mul = liftRR2 (*) -- RR $ x * y

    and_ = liftRR2 (&&) --RR $ x && y
    or_ = liftRR2 (||) --RR $ x || y

    --if_ (RR b) tc ec = if b then tc else ec
    if_ (RR b) (RR tc) (RR ec) = RR $ if b then tc else ec --RR applied to tc and ec

    leq = liftRR2 (<=)


--DATA is similar to newtype, but used
--when constructors has more than one field inside it

--TYPE  is a just allias for existing data type type Identifier = String

--NEWTYPE is similar to data, but only allows constuctors with one
--field in it

--CLASS is a TYPE CLASS!!!!, instance belong to the type class
-- a way to group similar types

--INSTANCE is instance of a type class

--Domain Specific Languages:
--Implemented by embedding within a general-purpose host language.
--Embedding may be deep or shallow
--Deep embedding:
    -- Language is defined in terms of syntax 
    -- Expressions are in data type
    -- Interpreter method eval is needed. (Semantics defined in it)

--Shallow embedding:
    -- Language is defined in terms of semantics
    -- Methods themselves implement semantics
    -- Said methods are defined in a type class
    -- and implemented in an instance of type class


\end{code}