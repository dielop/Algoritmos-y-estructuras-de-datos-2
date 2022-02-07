-- Las estructuras persistentes soportan un historial de versiones, es mas facil trabajar en paralelo.
data Bin a = H | N (Bin a) a (Bin a) deriving Show
--H = hoja
--N = nodo

minimum' :: Bin a -> a
minimum' (N H a r) = a
minimum' (N l a r) = minimum' l

maximum' :: Bin a -> a
maximum' (N l a H) = a
maximum' (N l a r) = maximum' r

--checkBST :: Bin a -> Bool
checkBST H = True
checkBST (N H a H) = True
checkBST (N l a H) = checkBST l && (a > (maximum' l))
checkBST (N H a r) = checkBST r && (a < (minimum' r))
checkBST (N l a r) = checkBST l && checkBST r && (a < minimum' r) && (a > maximum' l)

--Ejercicio 6

-- funcion altura
altura H = 0
altura (N l a r) = 1 + max (altura l) (altura r)

completo :: a -> Int -> Bin a

{-
 N H 3 H
   3
  / \
 H   H

   H = SOLA = altura 0, siempre que tengo Nodo tengo una altura
-}


completo x 0 = H 
completo x n = let t = (completo x (n-1))
                   in N t x t


-- ARBOL BALANCEADO 

balanceado x 0 = H
-- balanceado x 1 = Nodo Hoja x Hoja
-- si n es impar entonces n-1 es par
--tl = arbol izquierdo y tr = arbol derecho
balanceado x n | (odd n) = let t = balanceado x (div (n-1) 2)  -- esto suponiendo que n es impar
                               in N t x t
               | otherwise = let m = div (n-1) 2
                                 tl = balanceado x m
                                 tr = balanceado x (m+1)
                                 in N tl x tr


--EJERCICIO 8


-- definicion member alterada que realiza a lo sumo d + 1 comparaciones
-- member :: Ord a -> a -> Bin a -> Bool
member a H = False
member a (N l b r) | a == b = True
                   | a < b = member a l
                   | otherwise = member a r

member_aux a c H = a == c
member_aux a c (N l b r) | a >= b = member_aux a b r
                         | otherwise = member_aux a c l

member' a H = False
member' a (N l b r) = member_aux a b (N l b r)

--member aux mejora el peor caso pero empeora los demas casos, porque va hasta el final y recien ahi termina. 
--por lo tanto no es mejor que el member, solo lo de arriba (hace que todos los casos tengan el mismo costo)
--el member original tiene un peor caso pero el resto mejora.

