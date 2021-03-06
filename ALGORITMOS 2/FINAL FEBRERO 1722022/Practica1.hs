module Practica1 where

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
case' [x] = []
case' [] = []
case' (x:y:xs) = y : case' (x:xs)


-- c)
map' f [] = []
map' f(x:xs) = f x : map' f xs

-- d) 
listNumeros = 1 : 2 : 3 : []

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail x xs = map (+x) (tail xs)

-- g)
listmin xs = head (sort xs)

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = (f x) : (smap f xs)

{-
2. Definir las siguientes funciones y determinar su tipo:

a) five, que dado cualquier valor, devuelve 5
five :: Int -> Int
five x = 5

b) apply, que toma una función y un valor, y devuelve el resultado de
aplicar la función al valor dado

apply :: Int -> Int
apply x = five x 

c) identidad, la función identidad
identidad :: Int -> Int
identidad x = x 

d) first, que toma un par ordenado, y devuelve su primera componente
first :: Int -> Int -> Int
first x y = x

e) derive, que aproxima la derivada de una función dada en un punto dado
derive :: (Fractional a) => (a -> a) -> a -> a -> a 
derive f x h = (f (x+h) - f x)/ h  

f) sign, la función signo
sign :: Int -> Int
sign x | x > 0 = 1
       | x < 0 = (-1)
       | otherwise = 0

g) vabs, la función valor absoluto (usando sign y sin usarla)
vabs :: Int -> Int
vabs x = if x > 0 then x else
           if x < then (x) else 0


vabs' :: Int -> Int
vabs' x = sign x
           

h) pot, que toma un entero y un número, y devuelve el resultado de
elevar el segundo a la potencia dada por el primero
pot :: Int -> Int -> Int
pot x y = x ^ y

i) xor, el operador de disyunción exclusiva
xor :: Bool -> Bool -> Bool                           -- xor True True = False
xor x y | x == True && y == False = True              -- xor True False = True
        | x == False && y == True = True              -- xor False True = True 
        | otherwise = False                           -- xor False False = False 

j) max3, que toma tres números enteros y devuelve el máximo entre ellos
max3 :: Int -> Int -> Int -> Int
max3 x y z = if x > y && x > z then x else
               if y > z then y else z

k) swap, que toma un par y devuelve el par con sus componentes invertidas
-}

swap :: (Int,Int) -> (Int,Int)
swap (x,y) = (y,x)

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

esBisiesto :: Integer -> Bool   --tipo
esBisiesto x = multip4 && (not multip100 || multip400) where
               multip4 = mod x 4 == 0
               multip100 = mod x 100 == 0
               multip400 = mod x 400 == 0

¿Cuál es el tipo de la función definida?
esBisiesto :: Integer -> Bool   --tipo
-}

{-
4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
a) (Int -> Int) -> Int
b) Int -> (Int -> Int)
c) (Int -> Int) -> (Int -> Int)
d) Int -> Bool
e) Bool -> (Bool -> Bool)
f) (Int,Char) -> Bool
g) (Int,Int) -> Int
h) Int -> (Int,Int)
i) a -> Bool
j) a -> a
-}


{-
5) Definir las siguientes funciones usando listas por comprensión:

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
divisors' :: Int -> [Int]
divisors' num = if (num > 0) then concat[[x,-x]  | x <- [1...abs num], mod num x == 0] else [] 

b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'

c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'

(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
unique :: [Int] -> [Int]
-}



{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

{-
7) Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números

b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario

c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario

d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales

e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado

f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados

g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes

h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda

i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares

j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)

k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}

{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/
-}