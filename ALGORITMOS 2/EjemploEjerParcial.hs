{-
Un equipo de urgencias EU es un tipo de datos que tiene dos clases de prioridades sobre sus elementos (trabajos): rapidos y aplazables. Cada trabajo tiene un identificador único y una prioridad.
Asumiremos que los identificadores son números naturales, y las prioridades valores booleanos.
Además, si T es el tipo de los trabajos, dispondremos de las funciones id : T → Int y priority : T → Bool que devuelven el identificador y la prioridad de un determinado trabajo (true será rapido, y false aplazable).
Los equipos de urgencias soportan las siguientes operaciones:
emptyEU, que crea un EU vacı́o
isEmptyEU, que devuelve true si y sólo si el EU está vacı́o.
insertEU, que inserta un trabajo en un EU
quickEU, que dado x : EU, devuelve true si y sólo si x tiene trabajos rápidos.
firstEU, que dado un EU no vacı́o, retorna la el último trabajo rápido insertado. En caso que no haya trabajos rápidos, devuelve el último trabajo insertado.
deleteEU, que dado un EU no vacı́o, elimina el último trabajo rápido insertado. En caso que no haya trabajos rápidos, elimina el último trabajo insertado.
-}

data EU a = EmptyT | T Int Bool (EU a) deriving Show

id :: EU a -> Int
id (T x _ _) = x

priority :: EU a -> Bool
priority (T _ y _) = y

emptyEU :: EU a
emptyEU = EmptyT

isEmptyEU :: EU a -> Bool
isEmptyEU EmptyT = True
isEmptyEU _ = False

insertEU :: Int -> Bool -> EU a -> EU a
insertEU id pri (EmptyT) = T id pri (EmptyT)
insertEU id pri (T x y z) = T id pri (T x y z)

{- probar si funciona
insertEU :: Int -> Bool -> EU a -> EU a
insertEU T _ _ _ = T x y EU a
-}

confirm :: EU a -> Bool
confirm (T id pri EmptyT) = if pri==True then True else False
confirm (T id pri n) = if pri==True then True else confirm n 

deleteEU :: EU a -> EU a
deleteEU EmptyT = error "Error EU vacio"
deleteEU (T id pri EmptyT) | pri==True = EmptyT 
deleteEU t@(T id pri n) |(confirm t == True) = if pri==True then n else (T id pri (deleteEU n)) 
                        | otherwise = n

quickEU :: EU a -> Bool
quickEU EmptyT = True
quickEU (T x y z) = if y==True then (quickEU z) else False

firstEU :: EU a -> EU a   
firstEU EmptyT = error "Error EU vacio"
firstEU (T id pri EmptyT) | pri==True = EmptyT
firstEU t@(T id pri n) | (confirm t==True) = if pri==True then T id pri EmptyT else firstEU n
                       | otherwise = T id pri EmptyT 