-- ARBOL BST

data Bin a = H | N (Bin a) a (Bin a) deriving Show
--H = hoja
--N = nodo

member :: Ord a => a -> Bin a -> Bool
member a H = False
member a (N l b r ) | a == b = True
                    | a < b = member a l
                    | a > b = member a r

inorder :: Bin a -> [a]
inorder H = [ ]
inorder (N l a r ) = inorder l ++ [a ] ++ inorder r

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

insert :: Ord a => a -> Bin a -> Bin a
insert a H = N H a H
insert a (N l b r ) | a <= b = N (insert a l) b r
                    | otherwise = N l b (insert a r )

delete :: Ord a => a -> Bin a -> Bin a
delete _ H = H
delete z (N l b r ) | z < b = N (delete z l) b r
delete z (N l b r ) | z > b = N l b (delete z r )
delete z (N l b r ) | z == b = H
delete z (N l b r ) | z == b = r
delete z (N l b r ) | z == b = l
delete z (N l b r ) | z == b = let y = minimum' r
                               in N l y (delete y r )

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

-- ARBOL COMPLETO

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


