data EU a = EmptyT | T Int Bool (EU a) deriving (Show)

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
insertEU id p EmptyT = T id p (EmptyT)
insertEU id p (T x y z) = T id p (T x y z)

quickEU :: EU a -> Bool
quickEU EmptyT = True
quickEU (T x y z)   | y==True = quickEU z
                    | otherwise = False

confirm :: EU a -> Bool
confirm (T id p EmptyT)    | p==True = True
                           | otherwise = False
confirm (T id p c) | p==True = True
                        | otherwise = confirm c

firstEU :: EU a -> EU a
firstEU EmptyT = error "No esta definida para ser vacia"
firstEU (T id p EmptyT)     | p==True = T id p EmptyT
firstEU t@(T id p c)| (confirm t == True) = if p==True then T id p EmptyT else firstEU c
                    | otherwise = T id p EmptyT

deleteEU :: EU a -> EU a
deleteEU EmptyT = error "No esta definida para ser vacia"
deleteEU (T id p EmptyT)| p==True = EmptyT
deleteEU t@(T id p c)   | (confirm t == True) = if p==True then c else (T id p (deleteEU c))
                        | otherwise = c