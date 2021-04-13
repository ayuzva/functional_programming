{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, Rank2Types #-}

module A5ans where

import Prelude hiding ((>>), drop)

import Data.Bifunctor ( Bifunctor(first) )

import Language.Haskell.TH ( Q, TExp )
import Language.Haskell.TH.Syntax (Lift, mk_tup_name)

{------------------------------------------------------------------------------
-- Recalling our Forth-like stack-based language...

Take the StackMachine from tutorial 10, and augment it with just 
enough features to be able to implement the heart of FizzBuzz 

https://skilldrick.github.io/easyforth/ is a helpful resource here for
better understanding Forth.

Note that the JVM is a stack machine, as is postscript (the heart of PDF),
so this is like "assembler" for quite a few languages.
------------------------------------------------------------------------------}

-- The signature is completely given, to make things simpler:
class StackMachine stk where
    empty :: stk ()

    push :: Lift a => a -> stk s -> stk (a, s)
    drop :: stk (a, s) -> stk s

    swap :: stk (a, (b, s)) -> stk (b, (a, s))
    dup  :: stk (a, s) -> stk (a, (a, s))
    rot   :: stk (a, (b, (c, s))) -> stk (b, (c, (a, s)))
    rot23 :: stk (a, (b, (c, s))) -> stk (a, (c, (b, s)))

    -- 's' prefix to obvious things to avoid name clashes
    sadd :: Num a => stk (a, (a, s)) -> stk (a, s)
    smul :: Num a => stk (a, (a, s)) -> stk (a, s)
    sleq :: Ord a => stk (a, (a, s)) -> stk (Bool, s)
    seql :: Eq  a => stk (a, (a, s)) -> stk (Bool, s)
    smod :: Integral a => stk (a, (a, s)) -> stk (a, s)
    sand :: stk (Bool, (Bool, s)) -> stk (Bool, s)
    sor :: stk (Bool, (Bool, s)) -> stk (Bool, s)
    snot  :: stk (Bool, s) -> stk (Bool, s)

    sappend :: stk (String, (String, s)) -> stk (String, s)

    spair :: stk (a, (b, s)) -> stk ((a, b), s)
    unpair :: stk ((a, b), s) -> stk (a, (b, s))
    sfst  :: stk ((a, b), s) -> stk (a, s)
    ssnd  :: stk ((a, b), s) -> stk (b, s)

    skip :: stk s -> stk s

    ifThenElse :: stk (Bool, (a, (a, s))) -> stk (a, s)

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) = flip (.)

{------------------------------------------------------------------------------
-- Q1.a
------------------------------------------------------------------------------}

-- Write a program with the following signature that takes as input
-- an Int, and returns a Bool for if the input is 0 mod 3, and
-- the string " Fizz" if True, "" otherwise
fizz :: (StackMachine stk) => stk (Int, s) -> stk (Bool, (String, s))
fizz = push ""  >> push False >> spair >> swap >> push "Fizz" >> push True >> spair >> swap >> push 3 >> swap  >> smod >> push 0 >> seql >> ifThenElse >> unpair

-- Write a program with the following signature that takes as input
-- an Int, and returns a Bool for if the input is 0 mod 5, and
-- the string " Buzz" if True, "" otherwise
buzz :: (StackMachine stk) => stk (Int, s) -> stk (Bool, (String, s))
buzz = push ""  >> push False >> spair >> swap >> push "Buzz" >> push True >> spair >> swap >> push 5 >> swap >> smod >> push 0 >> seql >> ifThenElse >> unpair

-- Write a program with the following signature that takes as input
-- an Int, and returns the following:
-- let (b1, s1) the return of calling fizz
-- let (b2, s2) the return of calling buzz
-- the output will be (not (b1 or b2), s1 ++ s2)
-- this involves a lot of stack manipulation!  My version of this code
-- is 14 instructions long (but I don't guarantee that's optimal)
fizzbuzz :: (StackMachine stk) => stk (Int, s) -> stk (Bool, (String, s))
fizzbuzz = dup >> fizz >> spair >> swap >> buzz >> rot23 >> swap >> unpair >> rot23 >> sor >> snot >> rot >> sappend >> swap

{------------------------------------------------------------------------------
-- Q1.b
------------------------------------------------------------------------------}

-- implement an instance of StackMachine for R.
-- The following lift combinators are useful.
-- (No Applicative or Monad instance, as all functions are unary, in a sense)
newtype R a = R {unR :: a}

instance Functor R where
  fmap f = R . f . unR

liftR1 :: (a -> b) -> R (a, s) -> R (b, s)
liftR1 f = R . (\(x,y) -> (f x , y)) . unR

liftR2 :: (a -> b -> c) -> R (a, (b, s)) -> R (c, s)
liftR2 f (R (a, (b, s))) = R (f a b, s)

instance StackMachine R where
    empty = R () -- :: stk ()
    push x y = R (x, unR y) -- :: Lift a => a -> stk s -> stk (a, s)
    drop (R (_, y)) = R y -- :: stk (a, s) -> stk s

    swap (R (x, (y, z))) = R (y, (x, z)) -- :: stk (a, (b, s)) -> stk (b, (a, s))
    dup  (R (x, y)) = R (x, (x, y)) -- :: stk (a, s) -> stk (a, (a, s))
    rot  (R (x, (y, (z, s)))) = R (y, (z, (x, s))) -- :: stk (a, (b, (c, s))) -> stk (b, (c, (a, s)))
    rot23 (R (x, (y, (z, s)))) = R (x, (z, (y, s))) -- :: stk (a, (b, (c, s))) -> stk (a, (c, (b, s)))

    -- -- 's' prefix to obvious things to avoid name clashes
    sadd = liftR2 (+) -- :: Num a => stk (a, (a, s)) -> stk (a, s)
    smul = liftR2 (*)-- :: Num a => stk (a, (a, s)) -> stk (a, s)
    sleq = liftR2 (<=) -- :: Ord a => stk (a, (a, s)) -> stk (Bool, s)
    seql = liftR2 (==) -- :: Eq  a => stk (a, (a, s)) -> stk (Bool, s)
    smod = liftR2 (mod) -- :: Integral a => stk (a, (a, s)) -> stk (a, s)
    sand = liftR2 (&&)-- :: stk (Bool, (Bool, s)) -> stk (Bool, s)
    sor  = liftR2 (||)-- :: stk (Bool, (Bool, s)) -> stk (Bool, s)
    snot = liftR1 (not)-- :: stk (Bool, s) -> stk (Bool, s)

    sappend = liftR2 (++)-- :: stk (String, (String, s)) -> stk (String, s)

    spair  = liftR2 (,)-- :: stk (a, (b, s)) -> stk ((a, b), s)
    unpair (R ((x, y), z)) = R (x, (y, z)) -- :: stk ((a, b), s) -> stk (a, (b, s))
    sfst  = liftR1 fst-- :: stk ((a, b), s) -> stk (a, s)
    ssnd  = liftR1 snd--  :: stk ((a, b), s) -> stk (b, s)

    skip x = x -- :: stk s -> stk s

    --stk (Bool, (a, (a, s))) -> stk (a, s)
    ifThenElse (R (x, (y, (z, s)))) | x = R (y, s)
                                    | otherwise = R (z, s)

{------------------------------------------------------------------------------
-- Q1.c, for the R instance above
------------------------------------------------------------------------------}

-- Write a function that returns to the top of the stack as the result
stkEvalResult :: R (a,s) -> a
stkEvalResult = fst . unR

-- Write a function that returns to the String which is the 2nd-most top
-- of the stack as result
stkPrintEvalOutput :: R (a, (String, s)) -> String
stkPrintEvalOutput = stkEvalResult . drop 

{------------------------------------------------------------------------------
-- Q2

Implement a compiler. The following helper function is very handy.
Except for |empty|, ALL of the rest of the code looks like
  func  = clift1 [|| \XXX -> YYY ||]
where pattern-matching on tuples for XXX is crucial.
------------------------------------------------------------------------------}

data C a = C { unC :: Q (TExp a) }

clift1 :: Q (TExp (t -> a)) -> C t -> C a
clift1 g (C x) = C [|| $$g $$x ||]

instance StackMachine C where
    empty = C [|| () ||]-- :: stk ()

    push = \x -> clift1 [|| (,) x ||]-- :: Lift a => a -> stk s -> stk (a, s)
    drop = clift1 [|| snd ||]-- :: stk (a, s) -> stk s
--
    swap = clift1 [|| \(x, (y, z))->(y, (x, z)) ||]-- :: stk (a, (b, s)) -> stk (b, (a, s))
    dup = clift1 [|| \(x,y)->(x, (x, y)) ||]  -- :: stk (a, s) -> stk (a, (a, s))
    rot = clift1 [|| \(a, (b, (c, s)))->(b, (c, (a, s))) ||]--  :: stk (a, (b, (c, s))) -> stk (b, (c, (a, s)))
    rot23 = clift1 [|| \(a, (b, (c, s)))->(a, (c, (b, s))) ||] -- :: stk (a, (b, (c, s))) -> stk (a, (c, (b, s)))
--
    -- 's' prefix to obvious things to avoid name clashes
    sadd = clift1 [|| \(x, (y, z)) -> ((x + y), z) ||]  -- :: Num a => stk (a, (a, s)) -> stk (a, s)
    smul = clift1 [|| \(x, (y, z)) -> ((x * y), z) ||] -- :: Num a => stk (a, (a, s)) -> stk (a, s)
    sleq = clift1 [|| \(x, (y, z)) -> ((x <= y), z) ||]-- :: Ord a => stk (a, (a, s)) -> stk (Bool, s)
    seql = clift1 [|| \(x, (y, z)) -> ((x == y), z) ||] -- :: Eq  a => stk (a, (a, s)) -> stk (Bool, s)
    smod = clift1 [|| \(x, (y, z)) -> ((x `mod` y), z) ||] -- :: Integral a => stk (a, (a, s)) -> stk (a, s)
    sand = clift1 [|| \(x, (y, z)) -> ((x && y), z) ||]-- :: stk (Bool, (Bool, s)) -> stk (Bool, s)
    sor = clift1 [|| \(x, (y, z)) -> ((x || y), z) ||]-- :: stk (Bool, (Bool, s)) -> stk (Bool, s)
    snot = clift1 [|| \x -> (not $ fst x, snd x) ||]

    sappend = clift1 [|| \(x, (y, z)) -> ((x ++ y), z) ||] -- :: stk (String, (String, s)) -> stk (String, s)

    spair = clift1 [|| \(x, (y, z)) -> ((x, y), z) ||]-- :: stk (a, (b, s)) -> stk ((a, b), s)
    unpair = clift1 [|| \((x, y), z) -> (x, (y, z)) ||]--  :: stk ((a, b), s) -> stk (a, (b, s))
    sfst = clift1 [|| \((x, _), z) -> (x, z) ||] -- :: stk ((a, b), s) -> stk (a, s)
    ssnd = clift1 [|| \((_, y), z) -> (y, z) ||]-- :: stk ((a, b), s) -> stk (b, s)

    skip = clift1 [|| id ||] -- :: stk s -> stk s

    ifThenElse = clift1 [|| \(x, (y, (z, s))) -> ((if x then y else z), s) ||]-- :: stk (Bool, (a, (a, s))) -> stk (a, s)

-----------------------------------------------------------------

{-
  3
  Implement a partial de-compiler, meaning mapping from an
  instance of StackMachine to an Instance of classes
    IntSy, BoolSy, OrderSy, PairSy
  *only*, for the Mar18 version of the classes.

  Use RR below.  See the tutorial 10 material to get started.
-}
class IntSy (rep :: * -> *) where 
  int :: Integer -> rep Integer                   -- introduce
  add :: rep Integer -> rep Integer -> rep Integer -- computation rules
  sub :: rep Integer -> rep Integer -> rep Integer
  mul :: rep Integer -> rep Integer -> rep Integer

class BoolSy rep where
  bool :: Bool -> rep Bool
  if_ :: rep Bool -> rep a -> rep a -> rep a

  and_ :: rep Bool -> rep Bool -> rep Bool
  or_  :: rep Bool -> rep Bool -> rep Bool
  not_ :: rep Bool -> rep Bool

class OrderSy rep where
  leq :: rep Integer -> rep Integer -> rep Bool

class PairSy rep where
  pair :: rep a -> rep b -> rep (a , b) -- (,) lifted
  fst_ :: rep (a, b) -> rep a
  snd_ :: rep (a, b) -> rep b


newtype RR c a = RR { unRR :: forall s. c s -> c (a,s) }


instance StackMachine c => IntSy (RR c) where
  int = \x -> RR (push x) -- :: Integer -> rep Integer                   -- introduce
  add = \x y -> RR $ sadd . unRR x . unRR y -- :: rep Integer -> rep Integer -> rep Integer -- computation rules
  sub = \x y -> RR $ sadd . unRR x . smul . unRR y . unRR (int (-1))-- :: rep Integer -> rep Integer -> rep Integer
  mul = \x y -> RR $ smul . unRR x . unRR y-- :: rep Integer -> rep Integer -> rep Integer
--
instance StackMachine c => BoolSy (RR c) where
  bool = \x -> RR (push x) -- :: Bool -> rep Bool
  if_ = \x y z -> RR $ ifThenElse . unRR x . unRR y . unRR z -- :: rep Bool -> rep a -> rep a -> rep a
--
  and_ = \x y -> RR $ sand . unRR x . unRR y -- :: rep Bool -> rep Bool -> rep Bool
  or_ =  \x y -> RR $ sor . unRR x . unRR y -- :: rep Bool -> rep Bool -> rep Bool
  not_ = \x -> RR $ snot . unRR x  -- :: rep Bool -> rep Bool

instance StackMachine c => OrderSy (RR c) where
  leq = \x y -> RR $ sleq . unRR x . unRR y-- :: rep Integer -> rep Integer -> rep Bool

instance StackMachine c => PairSy (RR c) where
  pair = \x y -> RR $ (spair) . unRR x . unRR y -- :: rep a -> rep b -> rep (a , b) -- (,) lifted
  fst_ = \x -> RR $ sfst . unRR x -- :: rep (a, b) -> rep a
  snd_ = \x -> RR $ ssnd . unRR x -- :: rep (a, b) -> rep b


{- 4
  Write test cases for all of this:

  - 10 non-trivial programs in the StackMachine language. Each should
     have at least 8 instructions.  
  - write code that runs all these programs and check that the answer is
    correct.

  - pass all your programs through the C compiler as well. use |pprint|
     in a |main| program to print out the result code for each

  - for all the programs that can be decompiled (there should be at least 5),
      - run them through the RR interpreter, instantiated with the R interpreter
        and then the Mar18.R interpreter to "run" them
      - do the same for the PP instance of Mar22
      - (bonus) do the same for the PE instance of Apr??
-}
