{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
<<<<<<< HEAD
=======
{-# LANGUAGE FlexibleInstances #-}
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
module A4 where

-- The following are useful for the assignment
import Data.List (intersperse, nub)

-- Useful extra imports for the Bonus only
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader

-- Here is the class that the assignment is all about: a representation
-- of boolean expressions with variables.
-- Note: 
-- 1) true is represented as 'andB []' 
-- 2) false as 'orB []'
-- 3) 'andB [e]' means the same as 'e', same with 'orB [e]'.
class BoolExpr repr where
  varB :: String -> repr
  notB :: repr -> repr
  andB :: [repr] -> repr
  orB :: [repr] -> repr
  impliesB :: repr -> repr -> repr
  xorB :: [repr] -> repr

-- Some useful test cases:
ex1, ex2, ex3, ex4, ex5 :: BoolExpr repr => repr
ex1 = andB [andB [], notB $ varB "x", orB [varB "y", varB "z"]]
ex2 = andB [andB [], notB $ varB "x", orB [varB "y", varB "z", varB "x"]]
ex3 = andB [andB [orB [varB "w"]]]
ex4 = andB [varB "w"]
ex5 = andB []
-- You should add more test cases, enough to cover all the 'corner' cases,
-- and then run them through all your examples.

-- Zeroeth Question: why is it justified for andB, orB and xorB to take lists
-- as input?  What does |xorB []| mean?

<<<<<<< HEAD
=======
-- It is justifiable to do so as these operators are binary operators.
-- Therefore, they can be applied sequentially between next element of a list
-- and the result of application on previous elements. The operation will be 
-- folded.

-- |xorB []| is xor folded on elements of a list.

>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
----------------------------------------------------------------------------
-- First interpretation: as a String.
newtype Pr = Pr {view :: String}

instance BoolExpr Pr where
<<<<<<< HEAD
=======
  varB x = Pr ("var \"" ++ x ++ "\"") --String -> repr
  notB x = Pr ("(notB " ++ view x ++ ")") --repr -> repr
  andB x = Pr ("(andB [" ++ delimConcat (map view x) ++ "])")  --[repr] -> repr
  orB x =  Pr ("(orB [" ++ delimConcat (map view x) ++ "])") --[repr] -> repr
  impliesB x y = Pr("(impliesB " ++ view x ++ " " ++ view y ++ ")" ) --repr -> repr -> repr
  xorB x = Pr ("(xorB [" ++ delimConcat (map view x) ++ "])") --[repr] -> repr

delimConcat :: [String] -> String
delimConcat = foldr (\a b -> a ++ if b == "" then b else "," ++ b) ""
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164

-- Test case:
-- view ex2 should return the String
-- "(andB [(andB []),(notB var \"x\"),(orB [var \"y\",var \"z\",var \"x\"])])"
-- so that 'putStrLn $ view ex2' gives
-- (andB [(andB []),(notB var "x"),(orB [var "y",var "z",var "x"])])
-- Note that 'view' should not do any simplifications; even though ex3
-- and ex4 mean "the same", they will print differently.

----------------------------------------------------------------------------
-- Second interpretation: pulling out the (free) variables:
newtype FV = FV {fv :: [String]}

instance BoolExpr FV where
<<<<<<< HEAD
=======
  varB x = FV [x] -- String -> repr
  notB x = FV (fv x)  --repr -> repr
  andB x = FV (nub (foldr (++) [] (map fv x))) --[repr] -> repr
  orB x = FV (nub (foldr (++) [] (map fv x))) --[repr] -> repr
  impliesB x y = FV ((fv x) ++ (fv y))--repr -> repr -> repr
  xorB x = FV (nub (foldr (++) [] (map fv x))) --[repr] -> repr
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164

-- Test case:
-- fv ex2 should return exactly
-- [ "x", "y", "z" ]
-- Hint: Data.List.nub

----------------------------------------------------------------------------
-- Third interpretation: as 'syntax'
data BE = Var String | Not BE | And BE BE | Or BE BE | TrueB | FalseB
  deriving Show
asBE :: BE -> BE
asBE = id

-- Hint: this instance has more cases than the above
-- Hint: foldr1
instance BoolExpr BE where
<<<<<<< HEAD
=======
  varB x = Var x -- String -> repr
  notB x = Not x -- repr -> repr
  andB [] = TrueB  --[repr] -> repr
  andB x = foldr1 And x
  orB [] = FalseB
  orB x = foldr1 Or x -- [repr] -> repr
  impliesB x y = Or (Not x) (y)-- repr -> repr -> repr
  xorB x = foldr1 xorHelper x-- [repr] -> repr

xorHelper :: BE -> BE -> BE
xorHelper x y = Or (And x (Not y)) (And (Not x) y)
-- ex1 = andB [andB [], notB $ varB "x", orB [varB "y", varB "z"]]
-- ex2 = andB [andB [], notB $ varB "x", orB [varB "y", varB "z", varB "x"]]
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164

-- Test cases:
-- asBE ex1
-- And TrueB (And (Not (Var "x")) (Or (Var "y") (Var "z")))
-- asBE ex2
-- And TrueB (And (Not (Var "x")) (Or (Var "y") (Or (Var "z") (Var "x"))))
-- asBE ex3
-- Var "w"

----------------------------------------------------------------------------
-- Fourth question: the other direction!
toBoolExpr :: BoolExpr repr => BE -> repr
<<<<<<< HEAD
toBoolExpr _ = undefined
=======
toBoolExpr (Var x) = varB x
toBoolExpr (Not x) = notB (toBoolExpr x)
toBoolExpr (And x y) = andB ([(toBoolExpr x)] ++ [(toBoolExpr y)])
toBoolExpr (Or x y) = orB ([(toBoolExpr x)] ++ [(toBoolExpr y)])
toBoolExpr TrueB = andB []
toBoolExpr FalseB = orB []
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164

ex1b, ex2b, ex3b, ex4b, ex5b :: BE
ex1b = And TrueB (And (Not (Var "x")) (Or (Var "y") (Var "z")))
ex2b = And TrueB (And (Not (Var "x")) (Or (Var "y") (Or (Var "z") (Var "x"))))
ex3b = Var "w"
ex4b = Var "s"
ex5b = TrueB

-- Part of this question: give an example that shows that you can
-- go in both directions (between BR and (BoolExpr repr => repr))
-- but that the translation is _not_ the identity.
-- [i.e. add some code here, and then something in 'main' below
--  that clearly demonstrates it]
-- Hint: depth will, in general, change.
<<<<<<< HEAD

=======
exTranslation :: BoolExpr repr => repr
exTranslation = andB [varB "x", varB "y", varB "z"]
translated :: BE
translated = asBE exTranslation
retranslated :: BoolExpr repr => repr
retranslated = toBoolExpr translated
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
----------------------------------------------------------------------------
-- Fifth question: compute the 'size' of an expression.
-- More precisely: every 'constructor' of the BoolExpr language counts
-- for 1.
-- size ex1 = 7
-- size ex2 = 8
-- size ex3 = 4
-- size ex4 = 2
newtype Size = Sz {size :: Int}

instance BoolExpr Size where
<<<<<<< HEAD

=======
  varB _ = Sz 1 -- String -> repr
  notB x = Sz (1 + (size x)) -- repr -> repr
  andB x = Sz (1 + sum (map size x))
  orB x =  Sz (1 + sum (map size x)) -- [repr] -> repr
  impliesB x y = Sz (1 + (size x) + (size y))-- repr -> repr -> repr
  xorB x = Sz (1 + sum (map size x)) -- [repr] -> repr
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
----------------------------------------------------------------------------
-- Sixth question: compute the 'depth' of an expression (as a tree)
-- except that varB counts as 0 depth.
-- depth ex1 = 2
-- depth ex2 = 2
-- depth ex3 = 3
-- depth ex4 = 1
-- Hint: maximum
newtype Depth = De {depth :: Int}

instance BoolExpr Depth where
<<<<<<< HEAD
=======
  varB _ = De 0 -- String -> repr
  notB x = De (1 + (depth x)) -- repr -> repr
  andB [] = De 1
  andB x = De (1 + maximum (map depth x))
  orB [] = De 1
  orB x =  De (1 + maximum (map depth x)) -- [repr] -> repr
  impliesB x y = De (1 + (depth x) + (depth y))-- repr -> repr -> repr
  xorB [] = De 1
  xorB x = De (1 + maximum (map depth x)) -- [repr] -> repr
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164

-- Lastly, give an explicit example where going to BE and then back
-- to repr changes the depth of the results.

<<<<<<< HEAD
------------------------------------------------------------------------
-- Bonus questions
--

=======
-- andB [varB "x", varB "y", varB "z"]
-- The expression will start with a depth 1
-- but upon translating and retranslating will end up with depth 2.
-- This is due to repr And in BE being binary, but andB in repr being
-- fold of logic and over a list. 
------------------------------------------------------------------------
-- Bonus questions
--
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
-- Bonus 1: implement a (potentially failing) evaluator from
-- a Valuation (an assignment of Bool to variables).

-- Use the following definitions:
type Valuation = Map.Map String Bool

newtype NotFound = VarNotFound String

newtype Eval = Ev { val :: ExceptT NotFound (Reader Valuation) Bool}

-- Hint: ask, liftEither, sequence, Data.Foldable.and, and monads
instance BoolExpr Eval where
<<<<<<< HEAD
=======
    varB x = Ev $ do 
                    env <- ask
                    let left = (VarNotFound x)
                    let right = (Map.lookup x env)
                    result <- liftEither $ (maybe (Left left) (Right) right)
                    return result

    notB x = Ev $ do
                    values <- (val x)
                    return (not values)

    andB x = Ev $ do
                    values <- sequence (map val x)
                    return (and values)

    orB x = Ev $ do
                    values <- sequence (map val x)
                    return (or values)

    impliesB x y = Ev $ do
                        xVal <- val x
                        yVal <- val y
                        return ((not xVal) || yVal)
    xorB x = Ev $ do
                    values <- sequence (map val x)
                    let fstHalf = (or values)
                    let sndHalf = not (and values)
                    return (fstHalf && sndHalf)

--ask :: MonadReader r m => m r -- will give you a monad with environment in it
--liftEither :: MonadError e m => Either e a -> m a --defined in ExceptT
--sequence :: (Traversable t, Monad m) => t (m a) -> m (t a) --
--Data.Foldable.and :: Foldable t => t Bool -> Bool -- reutrns conjunction of container of bools
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164

-- For the rest of the bonus questions, the 'tutorial' at
-- http://okmij.org/ftp/tagless-final/index.html#course-oxford
-- contains a *lot* of useful advice. The most relevant is in
-- section 2, but section 3 and 4 are full of fascinating stuff too!
--
-- Bonus 2: implement another "printer" like that of Pr but minimize
-- the number of () in the results. 
<<<<<<< HEAD
=======

--data Pr' = Pr' {view' :: String, brackets' :: Bool}
data Context' = Brack | NoBrack

instance BoolExpr (Context'-> String) where
  varB x _ = ("var \"" ++ x ++ "\"")
  notB x _ = ("notB " ++ x NoBrack)

  andB [] _ = ("andB [" ++ "]")
  andB x NoBrack  = ("andB [" ++ delimConcat (map (\z -> z Brack) x) ++ "]")
  andB x Brack  = ("(andB [" ++ delimConcat (map (\z -> z Brack) x) ++ "])")

  orB [] _ = ("orB [" ++ "]")
  orB x NoBrack  = ("orB [" ++ delimConcat (map (\z -> z Brack) x) ++ "]")
  orB x Brack  = ("(orB [" ++ delimConcat (map (\z -> z Brack) x) ++ "])")

  impliesB x y _= ("(impliesB " ++ x Brack ++ " " ++ y Brack ++ ")" ) 

  xorB [] _ = ("xorB [" ++ "]")
  xorB x NoBrack  = ("xorB [" ++ delimConcat (map (\z -> z Brack) x) ++ "]")
  xorB x Brack  = ("(xorB [" ++ delimConcat (map (\z -> z Brack) x) ++ "])")

view' :: (Context' -> String) -> String
view' x = x NoBrack

>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
--
-- Bonus 3: change BExpr so that it becomes possible to implement the CNOT
-- gate that is useful in quantum computing.
--  https://en.wikipedia.org/wiki/Controlled_NOT_gate
<<<<<<< HEAD
=======

>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
--
-- Bonus 4: implement an interpretation of BoolExpr that 
-- pushes all the 'notB' to be only on variables.
-- Hint: see push_neg in the tutorial.
<<<<<<< HEAD
=======

ex1' :: BoolExpr repr => repr
ex1' = notB $ andB [andB [], notB $ varB "x", orB [varB "y", varB "z"]]

data Context = BoolPos | BoolNeg

instance BoolExpr repr => BoolExpr (Context -> repr) where
  varB x BoolPos = varB x
  varB x BoolNeg = notB (varB x)
  notB x BoolPos = x BoolNeg
  notB x BoolNeg = x BoolPos

  andB x BoolNeg = orB (map (\z -> z BoolNeg) x)
  andB x BoolPos = andB (map (\z -> z BoolPos) x)
  orB x BoolNeg = andB (map (\z -> z BoolNeg) x)
  orB x BoolPos = orB (map (\z -> z BoolPos) x)

  impliesB x y BoolNeg = andB [(x BoolPos), (y BoolNeg)] -- by negation of impl
  --impliesB converted to ANDs instead of implication,
  --for easier implementation of questions 5 and 6
  impliesB x y BoolPos = orB [(x BoolNeg), (y BoolPos)]
  
  xorB x BoolPos = xorB (map (\z -> z BoolPos) x)
  xorB x BoolNeg = andB [leftPart, rightPart]
                  where leftPart = andB (map (\z -> z BoolNeg) x)
                        rightPart = andB (map (\z -> z BoolPos) x)

push_neg :: BoolExpr repr => (Context -> repr) -> repr
push_neg e = (e BoolPos)
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
--
-- Bonus 5: convert a BoolExpr to an equivalent BoolExpr that is in
-- https://en.wikipedia.org/wiki/Conjunctive_normal_form
--
-- Bonus 6: convert a BoolExpr to an equivalent BoolExpr that is in
-- https://en.wikipedia.org/wiki/Disjunctive_normal_form
--
-- Bonus 7: 'simplify' a BoolExpr.  Use the following logical
-- formulas to implement *generalized* simplifications
<<<<<<< HEAD
-- 1. true and x <-> x
=======
-- 1. true and x <-> x -- andB [andB [], x]
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
-- 2. false or x <-> x
-- 3. x or x <-> x
-- 4. x and x <-> x
-- 5. false and y <-> false
-- 6. and & or are associative
<<<<<<< HEAD
=======

-- Unfortunately it does not work without Eq defined. I am not sure
--on how to proceed from here

newtype Simple rep = Sp { simplify :: rep}
  deriving Eq

instance (Eq rep, BoolExpr rep) => BoolExpr (Simple rep) where
  varB x = varB x
  notB x = Sp $ notB (simplify x)
  andB x = simplify5 $ simplify4 $ simplify1 x
  orB x = orB (simplify3 $ simplify2 x)
  impliesB x y = Sp $ impliesB (simplify x) (simplify y)
  xorB x = Sp $ xorB (map simplify x) 

simplify1 :: (Eq rep, BoolExpr rep) => [Simple rep] -> [Simple rep]
simplify1 x = filter (\z -> (simplify z) /= (andB [])) x 

simplify2 :: (Eq rep, BoolExpr rep) => [Simple rep] -> [Simple rep]
simplify2 x = filter (\z -> (simplify z) /= (orB [])) x 

simplify3 :: (Eq rep, BoolExpr rep) => [Simple rep] -> [Simple rep]
simplify3 x = nub x 

simplify4 :: (Eq rep, BoolExpr rep) => [Simple rep] -> [Simple rep]
simplify4 x = nub x 

simplify5 :: (Eq rep, BoolExpr rep) => [Simple rep] -> Simple rep
simplify5 x | test == [] = Sp $ andB (map simplify x)
               | otherwise = Sp $ andB []
               where 
                test = [z | z <- x, z == (Sp $ (orB []))]

>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
------------------------------------------------------------------------
-- You can hand in a filled-in A5.hs (i.e. named that), or A5.lhs 
-- or A5.org.

------------------------------------------------------------------------
main :: IO ()
main = do
  let exs  = [ex1, ex2, ex3, ex4, ex5]
  let exbs = [ex1b, ex2b, ex3b, ex4b, ex5b]
  putStrLn $ show $ map depth exs
  -- [2,2,3,1,1]
  putStrLn $ show $ map size exs
  -- [7,8,4,2,1]
  putStrLn $ show $ map view exs
  -- ["(andB [(andB []),(notB var \"x\"),(orB [var \"y\",var \"z\"])])","(andB [(andB []),(notB var \"x\"),(orB [var \"y\",var \"z\",var \"x\"])])","(andB [(andB [(orB [var \"w\"])])])","(andB [var \"s\"])","(andB [])"]

  putStrLn $ show $ map fv exs
  -- [["x","y","z"],["x","y","z"],["w"],["s"],[]]
  putStrLn $ show $ map asBE exs
  -- [And TrueB (And (Not (Var "x")) (Or (Var "y") (Var "z"))),And TrueB (And (Not (Var "x")) (Or (Var "y") (Or (Var "z") (Var "x")))),Var "w",Var "s",TrueB]
  putStrLn $ show $ map view $ map toBoolExpr exbs
  -- ["(andB [(andB []),(andB [(notB var \"x\"),(orB [var \"y\",var \"z\"])])])","(andB [(andB []),(andB [(notB var \"x\"),(orB [var \"y\",(orB [var \"z\",var \"x\"])])])])","var \"w\"","var \"s\"","(andB [])"]
<<<<<<< HEAD
=======

  --this show translating between BE and repr is not identity
  putStrLn ""
  putStrLn "This shows that rep -> BE -> rep is not an identity:"
  putStrLn $ view $ exTranslation
  putStrLn $ view $ retranslated
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
