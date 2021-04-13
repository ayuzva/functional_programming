
\begin{code}
data Edit =
    Insert Char
    | Change Char
    | Copy
    | Delete
    | Kill

run :: Edit -> String ->String
run (Insert c) s = c : s
run (Change _) [] = error "Can't change a character that is not there!"
run (Change c) (_ : s) = c : s
run Copy s = s
run Delete (_ : s) = s
run Kill _ = []

type Edits = [Edit]
runs :: Edits -> String -> String
runs [] s = s
runs (Insert c : cs) s       = c : runs cs s
runs (Change _ : _) []       = error "can't change a character that's not there"
runs (Change c : cs) (x : s) = c : runs cs s
runs (Copy    : cs) []       = error "can't copy no character"
runs (Copy    : cs) (x : s)  = x : runs cs s
runs (Delete  : cs) (x : s)  = runs cs s
runs (Kill    : cs) _        = runs cs []

runs' :: Edits -> String -> String
runs' [] s     = s
runs' (c:cs) s = x : runs' cs xs
    where
        (x, xs) = run c s


\end{code} 