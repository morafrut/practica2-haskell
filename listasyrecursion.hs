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
maximoLista (x:xs) = if x > maximoLista xs then x else maximoLista xs

-- 5. Índice con validación de rango:
indice :: [a] -> Int -> a
indice [] _ = error "Índice fuera de rango"
indice (x:xs) n =
    if n < 0 then
        error "Índice fuera de rango"
    else if n == 0 then
        x
    else
        indice xs (n-1)

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
