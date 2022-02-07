-------------------------TESTEO EJ PARCIAL
type Tr = (Int , Bool)
data Eu  a= E | T Tr (Eu  a) deriving Show
instance (Eq (Eu a)) where
    (==) E E = True
    (==) (T (x,y) _) (T (x1,y1) _) =  y1==y
    (==) _ _ = False


id::Eu  a-> Int
id ( T (a,_)_)= a
prioridad::Eu  a-> Bool
prioridad (T (_,p) _) = p

emptyEU:: Eu a
emptyEU = E

isEmptyEU:: Eu a ->Bool
isEmptyEU E = True
isEmptyEU _ = False

insertEU :: Int->Bool-> Eu  a-> Eu a
insertEU i p E = T (i,p) (E)
insertEU i p (T (x,y) z ) = T (i,p) (T (x,y) z )

quickEU:: Eu  a->Bool
quickEU E = True
quickEU (T (x,y) z )  | y==True = quickEU z
                      | otherwise = False

firstEU::Eu  a-> Tr
firstEU E = error"vacio"
firstEU (T (x,y) z)  | y==True =  (x,y)
                    | otherwise =  if( z==E) then (x,y) else firstEU z

deleteEU::  Eu  a-> Eu a
deleteEU E = error "No esta definido para vacio"
deleteEU (T (x,y) E )=  E
deleteEU ( T (x,y) z)    | (y==True) = z
                         | otherwise = if( z == E) then E else deleteEU z
