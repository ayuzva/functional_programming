<<<<<<< HEAD

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
=======

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
>>>>>>> 0d18276895fb62ed0f610dce5dbd85aa59016164
run 