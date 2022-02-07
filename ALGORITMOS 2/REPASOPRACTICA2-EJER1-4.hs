-- Ejercicio 1

type Color = (Int,Int,Int)

mezclar :: Color -> Color -> Color
mezclar (r1,g1,b1) (r2,g2,b2) = (div(r1+r2) 2, div(g1+g2) 2, div(b1+b2) 2)

-- Ejercicio 2

type Lineax = ([Char],[Char])

insertx e (xs,ys) = (e:xs,ys)

borrar (xs,ys) = (tail xs,ys)

--moverIzq ([],ys) = ([],ys), no matchea con el de abajo
moverIzq ((x:xs),ys) = (xs,x:ys)
-- para que matchee como el x:xs no es vacio, matchearia abajo
moverIzq l = l

--moverIni (xs,ys) = ([],ys) esto es redundante con la linea de abajo
moverIni (xs,ys) = ([], reverse xs ++ ys)
moverFin (xs,ys) = (reverse ys ++ xs,[])

vacia :: Lineax
vacia = ([],[])

--Ejercicio 3

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

cons e EmptyCL = CUnit e 
cons e (CUnit x) = Consnoc e EmptyCL x
cons e (Consnoc a l c) = Consnoc e (cons a l) c

snoc EmptyCL e = CUnit e
snoc (CUnit x) e = Consnoc x EmptyCL e
snoc (Consnoc a l c) e = Consnoc a (snoc l c) e 

headCL (CUnit x) = x
headCL (Consnoc a b c) = a

tailCL (CUnit x) = EmptyCL
tailCL (Consnoc a b c) = Consnoc (headCL b) (tailCL b) c

{-isEmptyCL (EmptyCL) = TRUE
--isEmptyCL (CUnit a) = FALSE

isCUnit (CUnit x) = TRUE
isCUnit (EmptyCL) = FALSE
-}
