\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE KindSignatures #-}
module Mar18 where

-- Note: this starts from the Mar16 file and then MODIFIES it

import Control.Applicative
import qualified Data.Function as Func (fix)

-- useful for the implementation below
if' :: Bool -> a -> a -> a
if' b x y = if b then x else y

\end{code}

Learning objectives:
\begin{itemize}
\item Typed Finally Tagless -- adding more features
\item  compositionality of languages
\item  traversals
\end{itemize}

\begin{code}
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

class SumSy rep where
  left :: rep a -> rep (Either a b)
  right :: rep b -> rep (Either a b)
  either_ :: rep (a -> c) -> rep (b -> c) -> rep (Either a b) -> rep c

class FunctionSy rep where
  app  :: rep (a -> b) -> (rep a -> rep b) -- ($) lifted
  lam  :: (rep a -> rep b) -> rep (a -> b) -- \ lifted

class FixSy rep where
  fix_ :: (rep (a -> b) -> rep (a -> b)) -> rep (a -> b)

class EqSy rep where
  eq :: Eq a => rep a -> rep a -> rep Bool
\end{code}

Augment the |R| instance with new features:
\begin{code}
newtype R a = R {unR :: a}

instance Functor R where
  -- fmap f x = R $ f $ unR x
  fmap f = R . f . unR

instance Applicative R where
  pure = R
  f <*> x = R $ (unR f) (unR x)

instance IntSy R where
  int  = pure
  add  = liftA2 (+)
  sub  = liftA2 (-)
  mul  = liftA2 (*)

instance BoolSy R where
  bool = pure
  and_ = liftA2 (&&)
  or_  = liftA2 (||)
  if_  = liftA3 (if')  -- elimination rule for Bool
  not_ = fmap not

instance OrderSy R where
  leq  = liftA2 (<=)

instance PairSy R where
  pair = liftA2 (,)   -- introduction rule
  fst_ = fmap   fst   -- elimination rule
  snd_ = fmap   snd   -- elimination rule

instance SumSy R where
  left = fmap   Left  -- introduction rule
  right = fmap  Right  -- introduction rule
  either_ = liftA3 either  -- elimination rule

instance FunctionSy R where
  app  = liftA2 ($)    -- elimination rule

  -- the "follow the types version":
  -- lam f = R $ \x -> unR $ f (R x)

  -- first put compositions on the rhs
  -- lam f = R $ \x -> unR . f . R $ x

  -- eta contract
  lam f = pure (unR . f . R)  -- introduction rule

instance FixSy R where
  fix_ f = R $ Func.fix (unR . f . R)

instance EqSy R where
  eq = liftA2 (==)
\end{code}

\begin{verbatim}
  fst_ is also
  fst_ x = R (fst $ unR x)
  fst_ x = R . fst . unR $ x
  fst_   = R . fst . unR
  fst_   = fmap fst
\end{verbatim}

\begin{code}
max' :: (BoolSy rep, OrderSy rep) => rep Integer -> rep Integer -> rep Integer
max' x y = if_ (leq x y) y x

max'' :: (FunctionSy rep, BoolSy rep, OrderSy rep) => 
  rep (Integer -> Integer -> Integer)
max'' = lam (\x -> lam (\y -> max' x y))

swap :: (FunctionSy rep, PairSy rep) => rep ((a, b) -> (b, a))
swap = lam (\x -> pair (snd_ x) (fst_ x))

fact :: (FixSy rep, FunctionSy rep, BoolSy rep, IntSy rep, OrderSy rep) =>
  rep (Integer -> Integer)
fact = fix_ (\self -> 
  lam (\n -> 
     if_ (leq n (int 1)) 
       (int 1) 
       (mul n (app self (add n (int (-1)))))))
\end{code}


And an interpreter for size:
\begin{code}
newtype Sz a = Sz {unSz :: Int}

instance Functor Sz where
  fmap _ x = Sz $ 1 + unSz x

instance Applicative Sz where
  pure = const $ Sz 1
  f <*> x = Sz $ unSz f + unSz x

instance IntSy Sz where
  int  = pure
  add  = liftA2 (+)
  sub  = liftA2 (-)
  mul  = liftA2 (*)

instance BoolSy Sz where
  bool = pure
  and_ = liftA2 (&&)
  or_  = liftA2 (||)
  not_ = fmap not

  if_  = liftA3 (if')

instance OrderSy Sz where
  leq  = liftA2 (<=)

instance FunctionSy Sz where
  app  = liftA2 ($)
  lam f = Sz $ 1 + (unSz . f . Sz $ 0)

instance PairSy Sz where
  pair = liftA2 ((,))
  fst_ = fmap fst
  snd_ = fmap snd

instance FixSy Sz where
  fix_ f = Sz $ unSz (f (Sz 0)) + 1
\end{code}

Testing:
\begin{code}
itest :: Integer
itest = unR $ app fact (int 5)

stest :: Int
stest = unSz fact
\end{code}
