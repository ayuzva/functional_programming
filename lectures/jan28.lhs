\begin{code}

module Jan28 where

data Expr =
    Lit Integer
    | Expr :+: Expr
    | Expr :-: Expr
    deriving Show

ee1 :: Expr
ee1 = (Lit 5) :+: ((Lit 6) :-: (Lit 22))

eval :: Expr -> Integer
eval (Lit i ) = i
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :-: e2) = eval e1 - eval e2

assoc :: Expr -> Expr
assoc ((e1 :+: e2) :+: e3) = e1 :+: (e2 :+: e3)
assoc (Lit x) = Lit x
assoc (e1 :-: e2) = assoc e1 :-: assoc e2
assoc (e1 :+: e2) = e1 :+: assoc e2


data Expr' =
    Lit' Integer
    | Add [Expr']
    | Sub' Expr' Expr'
    deriving Show

data Person = Adult Name Bio
            | Child Name

type Name = String --type synonym

data Bio = Parent String [Person]
         | NonParent String

children_names :: Person -> [Name]
children_names (Adult _ b ) = names b 
children_names (Child _) = []

the_name :: Person -> Name
the_name (Adult n _) = n
the_name (Child n) = n

names :: Bio -> [Name]
names (Parent _ l) = map the_name l
names (NonParent _) = []
\end{code}
