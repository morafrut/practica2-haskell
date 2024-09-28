--------------- Listas y recursión ---------------

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumaLista :: Num a => [a] -> a
sumaLista [] = 0              
sumaLista (x:xs) = x + sumaLista xs  

-- 3. AgregaElemento en un solo caso:
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento xs elem b = if b then elem : xs else xs ++ [elem]

-- 4. maximoLista sin la función max:
maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "Lista vacía"
maximoLista [x] = x
maximoLista (x:xs) = maximoAux x xs
  where
    maximoAux y [] = y
    maximoAux y (z:zs)
      | z > y     = maximoAux z zs
      | otherwise = maximoAux y zs

-- 5. Índice con validación de rango:
indice :: [a] -> Int -> a
indice xs n 
  | n < 0 || n >= longitud xs = error "Índice fuera de rango"
  | otherwise = indiceAux xs n
  where
    indiceAux (x:_) 0 = x
    indiceAux (_:xs) m = indiceAux xs (m-1)

--------------- Listas por comprensión ---------------
-- 6 y 8. Uso de mod en lugar de rem:
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y | y <- xs, y /= x]

-- 6 y 8. Uso de mod en lugar de rem:
numerosPares :: [Int] -> [Int]
numerosPares xs = [x | x <- xs, x `mod` 2 == 0]
