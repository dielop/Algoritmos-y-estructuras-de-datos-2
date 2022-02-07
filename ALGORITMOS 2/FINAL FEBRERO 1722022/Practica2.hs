--EJERCICIO 1
{- Rojo, verde y azul son los colores primarios.
   Hay que definir un tipo Color y una funcion mezclar -> y con esto obtener el promedio componente a componente
   Ejemplo: 
   RGB (0,0,0) Es el color negro
   RGB (255,0,0) Es el color rojo
   RGB (255,255,0) Es el color entre ROJO y VERDE = AMARILLO (color luz, no pigmento)
   RGB (0,255,0) Es el color verde
   RGB (0,0,255) Es el color azul
   
   Se llega hasta 255 porque es de 8 bit cada uno.
-}
-- definimos una tupla

type Color = (Int, Int, Int)

-- definimos la funcion mezclar

mezclar :: Color -> Color -> Color
mezclar (r1,g1,b1) (r2,g2,b2) = (div (r1+r2) 2, div (g1+g2) 2, div (b1+b2) 2)

{- Usamos div ya que necesitamos numeros enteros, porque si dividimos directamente nos da
numeros float y en el tipo color pusimos todos Int -}

{- otra forma de realizarlo es:
En vez de type puedo realizarlo con DATA.-}

{- data Color = RGB Int Int Int deriving Show
   mezclar (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (div (r1+r2) 2, div (g1+g2) 2, div (b1+b2) 2)

en la prueba seria:
mezclar (RGB 255 0 0) (RGB 0 255 0) y el resultado = RGB 127 127 0
-}

--EJERCICIO 2

type Linea = ([Char], Int)

vacia :: Linea
vacia = ([],0)

moverIzq :: Linea -> Linea
moverIzq (c, 0) = (c,0)
moverIzq (c, n) = (c,n-1)

moverDer :: Linea -> Linea
moverDer (c,n) | length c == n = (c,n)
               | otherwise = (c,n+1)

{-
Otra forma es con case
moverDer (c,n) = case (lenght c) of
                       n -> (c,n)
                       _ -> (c,n+1)
-}