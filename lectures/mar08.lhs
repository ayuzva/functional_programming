\begin{code}
module Mar08 where

--DSL Encodings
--Expression Problem
--Finally Tagless

--Embeddings:
--Shallow. (Jan 26 for RegExp). Embed as native functions.
--Deep. (Feb 4, 8 for RE) Embed as data-structure
--Tagless (today) Embed as interface. - i.e typeclass

--Start with a a simple expression language. First deep, then shallow, then tagless

data Expr = LInt Integer | Expr :+: Expr | Expr :*: Expr

eval :: Expr -> Integer
eval (LInt i) = i
eval (x :+: y) = eval x + eval y
eval (x :*: y) = eval x * eval y

e0 :: Expr
ex0 :: Integer
e0 = (LInt 5 :+: LInt 3) :*: LInt 8
ex0 = eval e0

--And now shallow:

lint :: Integer -> Integer
ladd :: Integer -> Integer -> Integer
lmul :: Integer -> Integer -> Integer

lint i = i
ladd x y = x + y 
lmul x y = x * y

--the implicit way
lint = id
ladd = (+)
lmul = (*)

ex1 :: Integer
ex1 = lmul (ladd (lint 5) (lint 3)) (lint 8)

--And now, as an interface
--class ExprSy e where 
--    int :: Integer -> e
--    add :: e -> e -> e
--    mul :: e -> e -> e

-- Give an implementation:
newtype R = R {unR :: Integer}

liftR2 :: (Integer -> Integer -> Integer) -> (R -> R -> R)
liftR2 f x y = R $ f (unR x) (unR y)

instance ExprSy R where
    -- obvious, most direct impl:
    --int i = R i
    --add (R i) (R j) = R (i + j) 
    --mul (R i) (R j) = R (i * j)

    --the more interesting implementation:
    int = R
    add x y = liftR2 (+)
    mul x y = liftR2 (*)

ex2 :: Integer
ex2 = unR $ mul (add (int 5) (int 3)) (int 8)


--But we can easily write 'other' interpretations ...
--Example: length. First deep, then tagless.

sizeE :: Expr -> Int
sizeE (LInt _) = 1
sizeE (x :+: y) = sizeE x + sizeE y + 1
sizeE (x :*: y) = sizeE x + sizeE y + 1
--(Recall: in Haskell Ints are small, Integers are abitrary sized)

p2 :: ExprSy e => e
p2 = mul (add (int 5) (int 3)) (int 8)

ex2 :: Integer
ex2 = unR p2

ex2' :: Int
ex2' = unS p2

newtype S = S {unS :: Int}

instance ExprSy S where
    int _ = S 1
    add (S i) (S j) = s (i + j + 1)
    mul (S i) (S j) = s (i + j + 1)

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
-- Deep version, skip shallow version, not interesting, still.
data BExpr = BLit Bool | BExpr :&&: BExpr | BExpr :||: BExpr | Not BExpr

evalB :: BExpr -> Bool
evalB (BLit b) = b
evalB (b1 :&&: b2) = evalB b1 && evalB b2
evalB (b1 :|||: b2) = evalB b1 || evalB b2
evalB (Not b) = not $ evalB b

class BExprSy e where
    blit :: Bool -> e
    band :: e -> e -> e
    bor :: e -> e -> e
    bnot :: e -> e -> e

liftB1 :: (Bool -> Bool) -> (B -> B)
liftB1 f = \x -> B $ f (unB x)

liftB2 :: (Bool -> Bool -> Bool) -> (B -> B -> B)
liftB2 f = \x y -> B $ f (unB x) (unB y)

instance ExprBSy B where
    blit = B
    band = liftB2 (&&)
    bor = liftB2 (||)
    bnot = liftB1 (not)

--other naming convention, use trailing underscore
data BorI = Bool Bool | Int Integer

data Lang3 = BExpr BExpr | IExpr IExpr | If_ BExpr Lang3 Lang3

eval3 :: Lang3 -> BorI
eval3 (BExpr be) = Bool $ evalB be
eval3 (IExpr ie) = Int $ eval ie
eval3 (If_ b thene elsee) = if bevaluated then eval3 thene else eval3 elsee 
    where
        bevaluated = evalB b

ex4 :: Lang3
ex4 = If_ (BLit True :||: BLit False) (BExpr $ BLit True) ( IExpr $ LInt 3)

ex5 :: Bool -> Lang3
ex5 = If_ (BLit b) (BExpr $ BLit True) (IExpr $ LInt 3)

--And the same for the calss version.

--class (BExprSy b, ExprSy i) => Lang3Sy b i e where
--    bool3 :: b -> e
--    int3  :: i -> e
--    if_   :: b -> e -> e -> e

--Another problem: |Lang 3| is untyped. Can we do better?

--First do a typed version using classes. Then back to Deep embedding.

--Also, for now, do a big language. Portemanteau Sy(ntax) + (Se)mantics
--rep short for 'representation'. Note that here rep :: * -> *

class Symantics rep where
    int :: Integer -> rep Integer
    add :: rep Integer -> rep Integer -> rep Integer

    bool :: Bool -> rep Bool
    and_ :: rep Bool -> rep Bool -> rep Bool
    or_ :: rep Bool -> rep Bool -> rep Bool
    if_ :: rep Bool -> rep a -> rep a -> rep a

    leq :: rep Integer -> rep Integer -> rep Bool


newtype RR a = RR {unRR :: a}

instance Symantics RR where
    int = RR 
    bool = RR

    add (RR x) (RR y) = RR $ x + y
    mul (RR x) (RR y) = RR $ x + y

    and_ (RR x) (RR y) = RR $ x && y
    or_ (RR x) (RR y) = RR $ x || y

    if_ (RR b) tc ec = if b then tc else ec

\end{code}

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

