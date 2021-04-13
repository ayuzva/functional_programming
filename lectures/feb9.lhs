<<<<<<< HEAD



map f (collapse t) == collapse (mapTree f t)

Proof:
map f (collapse Leaf a) == collapse (mapTree f Leaf a)
map f ([a])
[f a]

collapse (mapTree f Leaf a)
collapse (Leaf $ f a)
[f a]


map f (collapse (Nonde l a r)) == 
map f (collapse l ++ (a : collapse r))
map f collapse l ++ f a : map collapse r --induction hypothesis
collapse (mapTree f l) ++ (f a : collapse (mapTree f r))
collapse (Node (mapTree f l) (f a) (mapTree f r))
collapse (mapTree f (Node l a r))

forall re : RE. forall x \in enumerate re. toRecog re x == True

Easy:
    P : forall e1 e2 :: Expr, eval (Add e1 e2) == eval (Add e2 e1)

eval e1  :+: eval e2
eval e2 :+: eval e1
eval(Add e2 e1)

=======



map f (collapse t) == collapse (mapTree f t)

Proof:
map f (collapse Leaf a) == collapse (mapTree f Leaf a)
map f ([a])
[f a]

collapse (mapTree f Leaf a)
collapse (Leaf $ f a)
[f a]


map f (collapse (Nonde l a r)) == 
map f (collapse l ++ (a : collapse r))
map f collapse l ++ f a : map collapse r --induction hypothesis
collapse (mapTree f l) ++ (f a : collapse (mapTree f r))
collapse (Node (mapTree f l) (f a) (mapTree f r))
collapse (mapTree f (Node l a r))

forall re : RE. forall x \in enumerate re. toRecog re x == True

Easy:
    P : forall e1 e2 :: Expr, eval (Add e1 e2) == eval (Add e2 e1)

eval e1  :+: eval e2
eval e2 :+: eval e1
eval(Add e2 e1)

>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
