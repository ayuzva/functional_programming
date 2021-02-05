Assignment 2
3FP3
McMaster University
Andriy Yuzva
yuzvaa

\begin{code}
--{-# OPTIONS_GHC -Wall #-}
module A2_yuzvaa where
\end{code}

\begin{code}
-------------------------------Qustion 1-------------------------------
\end{code}
iter :: Integer -> (a -> a) -> (a -> a)
iter n f
    | n > 0 = f . iter (n - 1) f     --iter.1
    | otherwise = id                 --iter.2

Proof:
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

\begin{code}
-------------------------------Qustion 2-------------------------------
\end{code}

map f (ys ++ zs) = map f ys ++ map f zs

Proof:
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

\begin{code}
-------------------------------Qustion 3-------------------------------
\end{code}
concat (map (map f)  xs) = map f (concat xs)

concat :: [[ a ]] -> [ a ]
concat = foldr (++) []      −−concat.1

Proof:
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

\begin{code}
-------------------------------Qustion 4-------------------------------
\end{code}
filter p (filter q xs) = filter (p &&& q) xs

p &&& q = \x -> p x && q x

Definition of filter
filter :: (a -> Bool) -> [a] -> [a]
filter f []    = []
filter f (x:xs)
  | f x         = x : filter f xs
  | otherwise      = filter f xs

Proof:
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
            Case `q k`:
                Case `p k`:

                Case `otherwise`:


                filter p (k : filter q ks) = filter (p &&& q) k:ks
                    <Definition of filter `filter f (x:xs)`>
                
                
                By cases:
                Case `p k`:
                    k : filter p (filter q ks) = filter (p &&& q) k:ks
                Case `otherwise `:
                    filter p (filter q ks) = filter (p &&& q) k:ks
                        <Definition of filter `filter f (x:xs)` considering the case>
                    filter p (filter q ks) = filter (p &&& q) ks
                        <
            Case `otherwise`:
                Case `p k`:

                Case `otherwise`:


                filter p (filter q ks) = filter (p &&& q) k:ks
                    <Definition of filter `filter f (x:xs)` considering the case>
                filter p (filter q ks) = filter (p &&& q) ks
                    <Induction hypothesis>
                filter (p &&& q) ks = filter (p &&& q) ks
                    <Reflexivity of equality>
                True

            p &&& q = \x -> p x && q xs


\begin{code}
-------------------------------Qustion 5-------------------------------
--Double infix operators were used in this definition to save single operators for question 6
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
eval' (a :/: b) = ((eval' a) `div` (eval' b)) --`div` was used rathern than `/` as the later expects Fractional type (Stackoverflow)

\end{code}

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

\begin{code}
-------------------------------Qustion 8A-------------------------------

--join :: (a -> c) -> (b -> d) -> Either a b -> Either c d
--join x y (Left z) = Left (either x y z)
--join x y (Right z) = Right (either x y z)

\end{code}

\begin{code}
-------------------------------Qustion 8B-------------------------------
data GTree a = Leaf a | Gnode [GTree a]

count' :: GTree a -> Integer
count' (Leaf a) = 1
count' (Gnode []) = 0
count' (Gnode (x:xs)) = (count' x) + count' (Gnode xs)

depth' :: GTree a -> Integer
depth' (Leaf a) = 0
depth' (Gnode []) = 0
depth' (Gnode (x:xs)) = 1 + max (depth' x) (depth' (Gnode xs))


sum' :: GTree Integer -> Integer
sum' (Leaf x) = x
sum' (Gnode []) = 0
sum' (Gnode (x:xs)) = (sum' x) + (sum' (Gnode xs))

find' :: Eq a => GTree a -> a -> Bool
find' (Leaf x) y  | (x == y) = True
                  | otherwise = False
find' (Gnode [])   _ = False
find' (Gnode (x:xs)) y = (find' x y) && (find' (Gnode xs) y)

map' :: (a -> b) -> GTree a -> GTree b
map' f (Leaf x) = (Leaf (f x))
map' f (Gnode []) = (Gnode [])
map' f (Gnode (x:xs)) = (Gnode ([map' f x] ++ [map' f (Gnode xs)]))

flatten' :: GTree a -> [a]
flatten' (Leaf x) = []
flatten' (Gnode []) = []
flatten' (Gnode (x:xs)) = (flatten' x) ++ (flatten' (Gnode xs))
\end{code}