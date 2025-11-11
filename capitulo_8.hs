--- 8.1 Reconocimiento de permutaciones

--- Función auxiliar para borrar un elemento de una lista
borra :: Eq a => a -> [a] -> [a]
borra x [] = []
borra x (y:ys) | x == y = ys
               | otherwise = y : borra x ys

--- Verifica si una lista es permutación de otra
esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion [] [] = True
esPermutacion [] (y:ys) = False
esPermutacion (x:xs) ys = elem x ys && esPermutacion xs (borra x ys)

--- 8.2 Ordenación por inserción

--- Verifica si una lista está ordenada
ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [_] = True
ordenada (x:y:xs) = (x <= y) && ordenada (y:xs)

--- Inserta un elemento en una lista ordenada
inserta :: Ord a => a -> [a] -> [a]
inserta e [] = [e]
inserta e (x:xs)
  | e <= x = e:x:xs
  | otherwise = x : inserta e xs

--- Ordena una lista por inserción
ordenaPorInsercion :: Ord a => [a] -> [a]
ordenaPorInsercion [] = []
ordenaPorInsercion (x:xs) = inserta x (ordenaPorInsercion xs)

--- 8.3 El triángulo de Pascal

--- Función auxiliar para crear pares adyacentes
pares :: [a] -> [(a,a)]
pares [] = []
pares [x] = []
pares (x:y:xs) = (x,y) : pares (y:xs)

--- Calcula la n-ésima fila del triángulo de Pascal
pascal :: Integer -> [Integer]
pascal 1 = [1]
pascal n = [1] ++ [x+y | (x,y) <- pares (pascal (n-1))] ++ [1]

--- 8.4 Cálculo de primos mediante la criba de Eratóstenes

--- Elimina múltiplos de n de una lista
elimina :: Int -> [Int] -> [Int]
elimina n xs = [x | x <- xs, x `mod` n /= 0]

--- Aplica la criba de Eratóstenes
criba :: [Int] -> [Int]
criba [] = []
criba (n:ns) = n : criba (elimina n ns)

--- Lista de primos del 1 al 100
primos1a100 :: [Int]
primos1a100 = criba [2..100]

--- 8.5 Conjetura de Goldbach

--- Verifica si un número es primo en el rango 1-100
esPrimo100 :: Int -> Bool
esPrimo100 n = n `elem` primos1a100

--- Verifica si un número es suma de dos primos
esSuma2Primos100 :: Int -> Bool
esSuma2Primos100 n = 
  not (null [(a,b) | a <- primos1a100, 
                     b <- primos1a100, 
                     n == a+b])

--- Propiedad de Goldbach para números pares del 4 al 100
prop_Goldbach :: Bool
prop_Goldbach = null [n | n <- [4..100], even n, not (esSuma2Primos100 n)]

--- 8.6 Multiconjuntos

--- Verifica si todos los elementos de la primera lista están en la segunda
todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
todosOcurrenEn xs ys = and [x `elem` ys | x <- xs]

--- Verifica si dos listas tienen exactamente los mismos elementos
igualesElementos :: Eq a => [a] -> [a] -> Bool
igualesElementos xs ys = todosOcurrenEn xs ys && todosOcurrenEn ys xs

--- Cuenta las ocurrencias de un elemento en una lista
numeroOcurrencias :: Eq a => a -> [a] -> Int
numeroOcurrencias x xs = length [x' | x' <- xs, x == x']

--- 8.7 Posiciones

--- Obtiene la lista de pares (elemento, posición)
posiciones :: [a] -> [(a,Int)]
posiciones xs = zip xs [0..]

--- Obtiene la primera posición de un elemento en una lista
primeraPosicion :: Eq a => a -> [a] -> Int
primeraPosicion x xs = head [i | (x',i) <- posiciones xs, x' == x]

--- 8.8 Ternas pitagóricas

--- Genera ternas pitagóricas hasta n (versión básica)
ternasPitagoricas :: Int -> [(Int,Int,Int)]
ternasPitagoricas n = [(a,b,c) | a <- [1..n],
                                 b <- [a..n],
                                 c <- [b..n],
                                 a^2 + b^2 == c^2]

--- Versión más eficiente de ternas pitagóricas
ternasPitagoricas' :: Int -> [(Int,Int,Int)]
ternasPitagoricas' n = [(a,b,c) | a <- [1..n],
                                  b <- [a..n],
                                  let c2 = a^2 + b^2
                                      c = floor (sqrt (fromIntegral c2)),
                                  c <= n,
                                  c^2 == c2]
