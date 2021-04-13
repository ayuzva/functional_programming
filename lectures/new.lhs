
data Edit = 
    Insert Char
    | Change Char 
    | Copy 
    | Delete
    | Kill

run :: Edit -> String -> String
run (Insert c) s = c : s
run (Change _) [] = error
run (Change c) (_ : s) = c : s
run 