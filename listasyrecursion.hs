
--------------- Listas y recursión ---------------

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs) 

sumaLista :: Num a => [a] -> a
sumaLista [] = 0              
sumaLista (x:xs) = x + sumaLista xs  

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento xs elem True  = elem : xs  
agregaElemento xs elem False = xs ++ [elem]  

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [x] = x 
maximoLista (x:xs) = max x (maximoLista xs)

indice :: [a] -> Int -> a
indice (x:_) 0 = x
indice (_:xs) n = indice xs (n-1)

--------------- Listas por comprehensión ---------------
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `div` x * x == n]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y | y <- xs, y /= x]

numerosPares :: [Int] -> [Int]
numerosPares xs = [x | x <- xs, x `rem` 2 == 0]
