foo xs = [e | xs <- [[i | n <- [1..x], i <- [x]] | (i,x) <- zip [1..] xs], e <- xs]

foo2 xs = [e | xs <- [[i | n <- [1..y], i <- [x]] | (x,y) <- zip [1..] xs], e <- xs]

foo3 xs = [e | xs <- [[i | n <- [1..i], i <- [x]] | (i,x) <- zip [1..] xs], e <- xs]