{-
EJERCICIO 1) 
RGB (0,0,0) negro (ausencia de rojo, verde, etc..)
    (255,0,0) rojo
    (255,255,0) amarillo
-}

type Color = (Int, Int, Int)

mezclar :: Color -> Color -> Color
mezclar (r1,g1,b1) (r2,g2,b2) = (div(r1+r2) 2, div (g1+g2) 2 , div (b1+b2) 2)

{-
USANDO DATA


data Color2 = RGB Int Int Int deriving Show --Muestra los tipos no primitivos el deriving show, convierte en texto lo que escribi como constructor
--Otra opcion: data Color = RGB(Int,Int,Int)
mezclar (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (div (r1+r2) 2) (div(g1+g2) 2)
-}
{-
EJERCICIO 2
-}
type Linea = ([Char],Int)

insertar e ([],p) = ([e],1) -- si yo programe bien todo, esta funcion no deberia hacer falta, la agrego porque detecto un error o no confio en quien programo.
insertar e (cs,p) = (ins e cs p, p + 1) 

ins e cs 0 = e:cs
ins e (c:cs) p = c : ins e cs (p-1)
--ins e [] p = error "el valor del cursor no es correcto"

{-
ins 'x' "Hola" 3
ins 'x' ('H':"ola") 3
                      <def ins.28>
'H': ins 'x' "ola" 2
'H': ins 'x' ('o':"la") 2
                      <def ins.28>
'H': 'o' : ins 'x' "la" 1
'H': 'o' : ins 'x' ('l':'a') 1
                      <def ins.27 ins e cs 0 = e:cs>
'H': 'o' : 'l' ins 'x' 'a' 0
'H': 'o' : 'l' : 'x' : 'a'
-}
{-
IMPORTANTE ANOTAR DIJO EL PROFE ESTO:
Mi funcion no es completa, da un error que es patron no exhaustivo
-}

--borrar tengo que hacer aca

{-
Una manera mas sencilla de hacerlo
-}
{-insertx
"hola"
("oH","la")
"Hox_la"
("xoH","la")
--moverini
"_Hoxla"
("","Hoxla")
--moverfin
"Hoxla_"
("alxoH","")
--borrar
"Hoxl_a"
("lxoH","a")
--moverIzq
"Hox_la"
("oH","xla")
-}
type Lineax = ([Char],[Char])
insertx e (xs,ys) = (e:xs,ys)
borrar (xs,ys) = (tail xs,ys)

--moverIzq ([],ys) = ([],ys), no matchea con el de abajo
moverIzq ((x:xs),ys) = (xs,x:ys)
-- para que matchee como el x:xs no es vacio, matchearia abajo
moverIzq l = l

--moverIni (xs,ys) = ([],ys) esto es redundante con la linea de abajo
moverIni (xs,ys) = ([], reverse [xs] ++ [ys])
moverFin (xs,ys) = (reverse [ys] ++ [xs],[])



vacia :: Linea
vacia = ([],0)

moverIzq (c,0) = (c,0)
moverIzq (c,n) = (c,n-1)

moverDer (s,n) = case (length s) of
                       n -> (s,n)
                       _ -> (s,n+1)

{-
OTRA FORMA:
moverDer (s,n) | length s == n = (s,n)
               | otherwise = (s, n+1) 
-}

{-
EJERCICIO 3
-}
data Clist a = EmptyCL | Cunit a | Consnoc a (CList a) a deriving Show

cons e EmptyCL = Cunit e
cons e (Cunit x) = Consnoc e EmptyCL x
cons e (Consnoc a l c) = Consnoc e (cons a l) c

snoc EmptyCL e = Cunit e
snoc (Cunit x) e = Consnoc x EmptyCL e
snoc (Consnoc a l c) e = Consnoc a (snoc l c) e 

lastCL(Cunit z) = z
lastCL(Consnoc x y z) = z

dellast EmptyCL = EmptyCL
dellast (Cunit x) = EmptyCL
dellast (Consnoc a EmptyCL c) = (Cunit a)
dellast (Consnoc a l c) = Consnoc a (dellast l) (lastCL l)
{-
[] --> EmptyCL
[1] --> Cunit 1
[1,2] --> Consnoc 1 (EmptyCL) 2
[1,2,3] --> Consnoc 1 (Cunit 2) 3
-}
headCL (Cunit x) = x
headCL (Consnoc a b c) = a

tailCL (Cunit x) = EmptyCL
tailCL (Consnoc a b c) = Consnoc (haedCL b) (tailCL b) c

-- Init -> [1,2,3] -> [[],[1],[1,2],[1,2,3]]
borrarUltimo [x] = []
borrarUltimo (x:xs) = x: borrarUltimo xs

inits' [] = []
inits' xs = inits' (borrarUltimo xs) ++ [xs] -- si no lo pongo con los corchetes, concateno una lista con otra lista

initsCL EmptyCL = (Cunit EmptyCL)
initsCL xs = snoc (initsCL (dellast xs)) xs

list2CL [] = EmptyCL
list2CL [x] = Cunit x
list2CL (x:xs) = cons x (list2CL xs)

cl2List EmptyCL = []
cl2List (Cunit x) = [x]
cl2List (Consnoc a b c) = a:cl2List b ++ [c]

--- VIRGINIA
{-
initsr :: Clist a-> CList (Clist a)
initsr (EmptyCL) = Cunit (EmptyCL)
initsr (Cunit x) = Cosnoc EmptyCL EmptyCL (Cunit x)
initsr l = Cosnoc l x EmptyCL
                where
                x= initsr(reverseCL (tailCL(reverseCL l)))

inits :: CList a->CList (CList a) 
inits (xs) = reverseCL (initsr xs)

PONER TANTAS VECES REVERSE, TIENE COSTO COMPUTACIONAL, NO SE APRUEBA
-}

-- CONCATENACION DE LISTA VACIA
[] ++ ys = ys
xs ++ [] = xs
(x:xs) ++ ys = x : (xs ++ ys)

-- FORMA 1
concatCL (Consnoc a l c) ys = cons a (concatCL (snoc 1 c) ys)
-- FORMA 2
concatCL xs ys = cons (headCL xs) (concatCL (tailCL xs) ys)
-- FORMA 3
concatCL (Consnoc a l c) (Consnoc x y z) = Consnoc a (concatCL (snoc l c) (cons x y)) z

--EJERCICIO 4

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving Show
seval :: Aexp -> Maybe Int

--NOTA CONSULTA: (div 2 0) * 2 igual a nothing * 2

seval (Num x) = Just x
seval (Prod e1 e2) = case (seval e1) of 
	                    Just n1 -> case (seval e2) of
	                       	            Just n2 -> Just (n1 * n2)
	                       	            _ -> Nothing
	                    _ -> Nothing

seval (Div e1 e2) = case (seval e1) of
                        Just n1 -> case (seval e2) of
                                        Just 0 -> Nothing
                                        Just n2 -> Just (div n1 n2)
                        _ -> Nothing 