module Practica where

import Data.List

{-
1) Los siguientes códigos tienen errores, cargar el archivo 20.Practica.0.hs en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)
regla b = case b of
    True -> "Quedate en Casa"
    False -> "Qudate en Casa"

-- b)
case' [] = []
case' [x] = []
case' (x:y:xs) = y : case' (x:xs)

-- c)
map' f []        =  []
map' f (x:xs)     =  f x : map' f xs

-- d)
listNumeros = 1 : 2 : 3 : []

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail x xs = map (+x) (tail xs) -- consultar en el video

-- g)
listmin xs = head (sort xs)

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = (f x) : (smap f xs)

-- Ejericio 2
-- a) five, que dado cualquier valor, devuelve 5

five :: Int -> Int
five _ = 5

-- b) apply, que toma una función y un valor, y devuelve el resultado de


apply :: Int -> Int 
apply x = five x

-- c) identidad, la función identidad
identidad :: Int -> Int
identidad x = x


-- d) first, que toma un par ordenado, y devuelve su primera componente
first :: Int -> Int -> Int
first x y = x

-- f) sign, la función signo
sign :: Int -> Int
sign x  | x>0 = 1
        | x<0 = (-1)
        | otherwise = 0

-- g) vabs, la función valor absoluto (usando sign y sin usarla)
abs :: Int -> Int
abs x = if x > 0 then x else -x

-- h) pot, que toma un entero y un número, y devuelve el resultado de elevar el segundo a la potencia dada por el primero
pot :: Int -> Int -> Int
pot x y = x ^ y

-- i) xor, el operador de disyunción exclusiva

-- j) max3, que toma tres números enteros y devuelve el máximo entre llos

--k) swap, que toma un par y devuelve el par con sus componentes invertidas

-- 5)
-- a) 'divisors', que dado un entero positivo 'x' devuelve la
-- lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
--divisors :: Int -> [Int]
--divisors num = if (num > 0) then concat[[x,-x] | x <- [1..abs num], mod num x == 0] else []

-- b) 'matches', que dados un entero 'x' y una lista de enteros descarta de la lista los elementos distintos a 'x'
matches :: Int -> [Int] -> [Int]
matches e l = [x | x <- l, x /= e]