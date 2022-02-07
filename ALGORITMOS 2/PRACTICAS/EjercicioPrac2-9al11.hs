--Ejercicio 9
{- RBT. Log en base 2 de la cantidad de elementos de la lista me da la altura.
Si la altura es par por ejemplo 8, si empiezo a pintar de rojo, el ultimo nivel COMPLETO lo pinto de negro
Si la altura es impar, me conviene empezar de negro en RBT y el ultimo nivel COMPLETO lo pinto de negro.
-}

fromList xs = makeBlack (if (even (truncate (logBase 2 (fromIntegral(length xs))))) -- una forma de reemplazar los parentesis es con . o $, ejemplo (even.truncate.logBase 2.fromIntegral.length xs)
                  fromList' xs R else fromList' xs B

fromList' [] = E
fromList' xs c = let n = div (length xs) 2
                  x = xs!!n --!! avanza n elementos en la lista y devuelve el elemento donde se posiciona
                  ant = take n xs -- si n = 2 me va a dar los primeros dos elementos de la lista (ej list 1,2,3,4,5)
                  pos = drop (n+1) xs -- n+1 porque descarto los primeros dos y el tercero que es el del medio
                  c' = if c == R then B else R
                  in T c (fromList' ant c') x (fromList' pos c')


--Ejercicio 10

insert :: Ord a -> a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
    where ins x E = T R E x E
ins x (T c l y r) | x < y = balancel c (ins x l) y r
                  | x > y = balancer c l y (ins x r)
                  | otherwise = T c l y r

makeBlack E = E
makeBlack (T l x r) = T B l x r

balancel :: Color -> RBT a -> a -> RBT a 0 -> RBT a
balancel B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balancel B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balancel c l a r = T c l a r

balancer :: Color -> RBT a -> a -> RBT a 0 -> RBT a
balancer B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balancer B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balancer c l a r = T c l a r

--Ejercicio 11

type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a) deriving Show

fromList1 xs = let ys = map (\x -> N 0 x E E) xs
                  pares [] = []
                  pares [x] = [x]
                  pares (x:y:hs) = merge x y : pares hs
                  g [h] = h
                  g hs = g (pares hs) 
               in g ys

-- Para que esto funcione tengo que tener definido el merge

{-
N _ 2 E E
[3,2,4]

 NOTA: RANGO ES LA LONGITUD DE LA ESPINA DERECHA.
[N 0 3 E E, N 0 2 E E, N 0 4 E E]
merge (merge (N 0 3 E E) (N 0 2 E E) (N 0 4 E E))

UNO MAS LARGO
[3,2,4,5,1,7]
-- merge (merge (merge (merge(merge 3 2) 4) 5) 1) 7 (asi no se puede hacer porque se hacen muchos merge)
[merge 3 2, merge 4 5, merge 1 7]
[(2,3),(4,5),(1,7)]
[merge (2,3) (4,5) (1,7)]
[((2,3),(4,5)),(1,7)]
merge ((2,3),(4,5)) (1,7)
-}

{-
NOTA:
Un heap por si solo no tiene que ser balanceado, ahora si hablamos de los leftist heaps tienen que ser balanceados
-}