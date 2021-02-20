Assignment 2
3FP3
McMaster University
Andriy Yuzva
yuzvaa

\begin{code}
{-# OPTIONS_GHC -Wall #-}
module A2_yuzvaa where

-------------------------------Qustion 1-------------------------------
\end{code}
Proving `iter n id = id`:

Given definition of iter:
iter :: Integer -> (a -> a) -> (a -> a)
iter n f
    | n > 0 = f . iter (n - 1) f     --iter.1
    | otherwise = id                 --iter.2

Proof via weak induction:
    For all n:
        By induction on `n`:
            Base case P(0):
                iter 0 id = id
                    <Definition of iter `iter.2`>
                id = id
                    <Reflexivity of equality>
                True    
            Induction step P(n+1): --Induction hypothesis: iter n id = id
                iter (n + 1) id = id
                    <Definition of iter `iter.1`>
                id . iter ((n + 1) - 1) id
                    <Evaluation>
                id . iter (n) id
                    <Induction hypothesis>
                id . id = id
                    <Definition of id>
                id = id
                    <Reflexivity of equality>
                True     

In conclusion, `iter n id = id` was proven
by weak induction.

\begin{code}
-------------------------------Qustion 2-------------------------------
\end{code}
Proving:
map f (ys ++ zs) = map f ys ++ map f zs

Proof via structural induction:
    For all ys, zs:
        By induction on `ys`:
            Base case P([]):
                map f ([] ++ zs) = map f [] ++ map f zs
                    <Concatenating empty list, Definition of map>
                map f (zs) = [] ++ map f zs
                    <Concatenating empty list>
                map f zs = map f zs
                    <Reflexivity of equality>
                True
            Induction step P(x:xs):     --Induction hypothesis: map f (ys ++ zs) = map f ys ++ map f zs
                map f ((x:xs) ++ zs) = map f (x:xs) ++ map f zs
                    <Mutual associativity of :>
                map f (x:(xs ++ zs)) = map f (x:xs) ++ map f zs
                    <Definition of map>
                (f x ) : map f (xs ++ zs) = map f (x:xs) ++ map f zs
                    <Induction hypothesis>
                (f x) : (map f ys ++ map f zs) = map f (x:xs) ++ map f zs
                    <Mutual associativity of :>
                ((f x) : (map f ys)) ++ map f zs = map f (x:xs) ++ map f zs
                    <Definition of map>
                map f (x:xs) ++ map f zs = map f (x:xs) ++ map f zs
                    <Reflexivity of equality>
                True

In conclusion, `map f (ys ++ zs) = map f ys ++ map f zs` was proven
by structural induction.

\begin{code}
-------------------------------Qustion 3-------------------------------
\end{code}
Proving:
concat (map (map f)  xs) = map f (concat xs)

Given:
concat :: [[ a ]] -> [ a ]
concat = foldr (++) []      −−concat.1

Proof via structural induction:
    By indcution on `xs`:
        Base case xs = []:
            concat (map (map f) []) = map f (concat [])
                <Definition of map: `map _ [] = []`>
            concat [] = map f (concat [])
                <Definition of concat: `concat = foldr (++) []`>
            foldr (++) [] [] = map f (concat [])
                <Definition of foldr `foldr f s [] = s`>
            [] = map f (concat [])
                <Definition of concat: `concat = foldr (++) []`>
            [] = map f (foldr (++) [] [])
                <Definition of foldr `foldr f s [] = s`>
            [] = map f ([])
                <Definition of map: `map _ [] = []`>
            [] = []
                <Reflexivity of equality>
            True
        Induction step xs = j:js:   --Induction hypothesis: concat (map (map f)  xs) = map f (concat xs)
            concat (map (map f)  j:js) = map f (concat j:js)
                <Definition of concat: `concat = foldr (++) []`>
            foldr (++) [] (map (map f)  j:js) = map f (concat j:js)
                <Definition of map `map f x:xs = (f x) : (map f xs)`
            foldr (++) [] ((map f j) : (map (map f) js)) = map f (concat j:js)            
                <Definition of foldr `foldr f s (x:xs) = x `f` (foldr f s xs)`>
            (map f j) ++ (foldr (++) [] (map (map f) js)) = map f (concat j:js)
                <Definition of concat `concat = foldr (++) []`>
            (map f j) ++ (concat (map (map f) js)) = map f (concat j:js)
                <Induction hypothesis>
            (map f j) ++ (map f (concat js)) = map f (concat j:js)
                <Distributivity of map>
            map f (j ++ concat js) = map f (concat j:js)
                <Definition of concat: `concat = foldr (++) []`>
            map f (j ++ foldr (++) [] js) = map f (concat j:js)
                <Definition of foldr `foldr f s (x:xs) = x `f` (foldr f s xs)`>
            map f (foldr (++) [] j:js) = map f (concat j:js)
                <Definition of concat: `concat = foldr (++) []`>
            map f (concat j:js) = map f (concat j:js)
                <Reflexivity of equality>
            True

In conclusion, `concat (map (map f)  xs) = map f (concat xs)` was proven
by structural induction.

\begin{code}
-------------------------------Qustion 4-------------------------------
\end{code}
Prove:
filter p (filter q xs) = filter (p &&& q) xs
for all finite lists xs where:
p &&& q = \x -> p x && q x

Definition of filter:
filter :: (a -> Bool) -> [a] -> [a]
filter f []    = []
filter f (x:xs)
  | f x         = x : filter f xs
  | otherwise      = filter f xs

Proof via structural induction:
    By induction on `xs`:
        Base case `xs = []`:
            filter p (filter q []) = filter (p &&& q) []
                <Definition of filter `filter f []    = []`>
            filter p [] = []
                <Definition of filter `filter f [] = []`>
            [] = []
                <Reflexivity of equality>
            True
        Induction step `xs = k:ks`:       --Induction hypothesis filter p (filter q xs) = filter (p &&& q) xs
            filter p (filter q k:ks) = filter (p &&& q) k:ks
                <Definition of filter `filter f (x:xs)`>
            By cases:
            Case `q k = True`:
                Case `p k = True`:
                    filter p (filter q k:ks) = filter (p &&& q) k:ks
                        <Definition of filter `filter f (x:xs)` considering the case>
                    filter p (k : filter q ks) = filter (p &&& q) k:ks
                        <Definition of filter `filter f (x:xs)` considering the case>
                    k : filter p (filter q ks) = filter (p &&& q) k:ks
                        <Induction hypothesis>
                    k : filter (p &&& q) ks = filter (p &&& q) k:ks
                        <Definition of lambda function considering the case:
                            (p &&& q) k = True
                        Combined with filter definition `filter f (x:xs)`>
                    filter (p &&& q) k:ks = filter (p &&& q) k:ks
                        <Reflexivity of equality>
                    True
                Case `p k = False`:
                    filter p (filter q k:ks) = filter (p &&& q) k:ks
                        <Definition of filter `filter f (x:xs)` considering the case>
                    filter p (k : filter q ks) = filter (p &&& q) k:ks
                        <Definition of filter `filter f (x:xs)` considering the case>
                    filter p (filter q ks) = filter (p &&& q) k:ks
                        <Induction hypothesis>
                    filter (p &&& q) ks = filter (p &&& q) k:ks
                        <Definition of lambda function considering the case:
                            (p &&& q) k = False
                        Combined with filter definition `filter f (x:xs)`>
                    filter (p &&& q) k:ks = filter (p &&& q) k:ks
                        <Reflexivity of equality>
                    True
            Case `q k = False`:
                Case `p k = True`:
                    filter p (filter q k:ks) = filter (p &&& q) k:ks
                        <Definition of filter `filter f (x:xs)` considering the case>
                    filter p (filter q ks) = filter (p &&& q) k:ks
                        <Induction hypothesis>
                    filter (p &&& q) ks = filter (p &&& q) k:ks
                        <Definition of lambda function considering the case:
                            (p &&& q) k = False
                        Combined with filter definition `filter f (x:xs)`>
                    filter (p &&& q) k:ks = filter (p &&& q) k:ks
                        <Reflexivity of equality>
                    True
                Case `p k = False`:
                    filter p (filter q k:ks) = filter (p &&& q) k:ks
                        <Definition of filter `filter f (x:xs)` considering the case>
                    filter p (filter q ks) = filter (p &&& q) k:ks
                        <Induction hypothesis>
                    filter (p &&& q) ks = filter (p &&& q) k:ks
                        <Definition of lambda function considering the case:
                            (p &&& q) k = False
                        Combined with filter definition `filter f (x:xs)`>
                    filter (p &&& q) k:ks = filter (p &&& q) k:ks
                        <Reflexivity of equality>
                    True

In conclusion, all four cases result in equality of LHS and RHS. Therefore 
`filter p (filter q xs) = filter (p &&& q)` xs was proven by structural induction.

\begin{code}
-------------------------------Qustion 5-------------------------------
--Double infix operators were used in this definition to reserve
--single operators for question 6
data Expr = 
    Lit Integer
    | Expr :++: Expr
    | Expr :--: Expr

size :: Expr -> Integer 
size (Lit _) = 0
size (a :++: b) = 1 + (size a) + (size b)
size (a :--: b) = 1 + (size a) + (size b)

\end{code}

\begin{code}
-------------------------------Qustion 6-------------------------------
data Expr' = 
    Lit' Integer
    | Expr' :+: Expr'
    | Expr' :-: Expr'
    | Expr' :*: Expr'
    | Expr' :/: Expr'

show' :: Expr' -> String
show' (Lit' a) = show a
show' (a :+: b) = "(" ++ (show' a) ++ " + " ++ (show' b) ++ ")"
show' (a :-: b) = "(" ++ (show' a) ++ " - " ++ (show' b) ++ ")"
show' (a :*: b) = "(" ++ (show' a) ++ " * " ++ (show' b) ++ ")"
show' (a :/: b) = "(" ++ (show' a) ++ " / " ++ (show' b)  ++ ")"

size' :: Expr' -> Integer 
size' (Lit' _) = 0
size' (a :+: b) = 1 + (size' a) + (size' b)
size' (a :-: b) = 1 + (size' a) + (size' b)
size' (a :*: b) = 1 + (size' a) + (size' b)
size' (a :/: b) = 1 + (size' a) + (size' b)

eval' :: Expr' -> Integer 
eval' (Lit' a) = a
eval' (a :+: b) = (eval' a) + (eval' b)
eval' (a :-: b) = (eval' a) - (eval' b)
eval' (a :*: b) = (eval' a) * (eval' b)
--eval' (a :/: Lit' 0) = error "div by zero"
eval' (a :/: b) = ((eval' a) `div` (eval' b)) --`div` was used rathern than `/` as the later expects Fractional type (Discussion Posts)

\end{code}
The function raises "divide by zero" exception. This is inherited from 'div' operator.
However, it is also possible to define custom exception, as seen above.
It is commented out due to 'div' already having this functionality.

\begin{code}
-------------------------------Qustion 7-------------------------------
data Expr2 = 
    Lit2 Integer 
    | Op Ops Expr2 Expr2

data Ops = Add | Sub | Mul | Div

show'' :: Expr2 -> String
show'' (Lit2 a) = show a
show'' (Op Add a b) = "(" ++ (show'' a) ++ " + " ++ (show'' b) ++ ")"
show'' (Op Sub a b)  = "(" ++ (show'' a) ++ " - " ++ (show'' b) ++ ")"
show'' (Op Mul a b)  = "(" ++ (show'' a) ++ " * " ++ (show'' b) ++ ")"
show'' (Op Div a b)  = "(" ++ (show'' a) ++ " / " ++ (show'' b)  ++ ")"

size'' :: Expr2 -> Integer 
size'' (Lit2 _) = 0
size'' (Op _ a b) = 1 + (size'' a) + (size'' b)

eval'' :: Expr2 -> Integer 
eval'' (Lit2 a) = a
eval'' (Op Add a b) = (eval'' a) + (eval'' b)
eval'' (Op Sub a b) = (eval'' a) - (eval'' b)
eval'' (Op Mul a b) = (eval'' a) * (eval'' b)
eval'' (Op Div a b) = ((eval'' a) `div` (eval'' b))

\end{code}
To add a Mod operation, firstly the data type Ops would need to have Mod added to it.
In terms of functions, the show' function will just need a separate pattern for Mod. 
Size function will not need to be modified. Eval will need a separate pattern for Mod.

\begin{code}
-------------------------------Qustion 8A-------------------------------
--Either is defined as:
--either                  :: (a -> c) -> (b -> c) -> Either a b -> c
--either f _ (Left x)     =  f x
--either _ g (Right y)    =  g y

join :: (a -> c) -> (b -> d) -> Either a b -> Either c d
join x _ (Left z) = Left (x z)
join _ y (Right z) = Right (y z)

\end{code}

\begin{code}
-------------------------------Qustion 8B-------------------------------
data GTree a = Leaf a | Gnode [GTree a] deriving Show

count' :: GTree a -> Integer
count' (Leaf _) = 1
count' (Gnode []) = 0
count' (Gnode (x:xs)) = foldr (+) (count' x) (map count' xs)

depth' :: GTree a -> Integer
depth' (Leaf _) = 0
depth' (Gnode []) = 0
depth' (Gnode (x:xs)) = 1 + foldr max (depth' x) (map depth' xs)


sum' :: GTree Integer -> Integer
sum' (Leaf x) = x
sum' (Gnode []) = 0
sum' (Gnode (x:xs)) = foldr (+) (sum' x) (map sum' xs) 

find' :: Eq a => a -> GTree a -> Bool
find' x (Leaf y)  | (x == y) = True
                  | otherwise = False
find' _ (Gnode [])  = False
find' x (Gnode (y:ys)) = foldr (||) (find' x y) (map (find' x) ys)

map' :: (a -> b) -> GTree a -> GTree b
map' f (Leaf x) = (Leaf (f x))
map' _ (Gnode []) = (Gnode [])
map' f (Gnode (x:xs)) = (Gnode ([map' f x] ++ map (map' f) xs))

flatten' :: GTree a -> [a]
flatten' (Leaf x) = [x]
flatten' (Gnode []) = []
flatten' (Gnode (x:xs)) = foldl (++) (flatten' x) (map flatten' xs) 
\end{code}