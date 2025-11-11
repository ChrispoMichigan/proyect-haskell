import Data.List ((\\))

--- 4.1 Segmentos iniciales
--- Obtiene la lista de todos los segmentos iniciales de una lista
iniciales :: [a] -> [[a]]
iniciales [] = [[]]
iniciales (x:xs) = [] : [x:ys | ys <- iniciales xs]

--- 4.2 Segmentos finales
--- Obtiene la lista de todos los segmentos finales de una lista
finales :: [a] -> [[a]]
finales [] = [[]]
finales (x:xs) = (x:xs) : finales xs

--- 4.3 Segmentos
--- Obtiene todos los segmentos de una lista
segmentos :: [a] -> [[a]]
segmentos [] = [[]]
segmentos (x:xs) = segmentos xs ++ [x:ys | ys <- iniciales xs]

--- 4.4 Sublistas
--- Obtiene todas las sublistas posibles de una lista
sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = [x:ys | ys <- sub] ++ sub
  where sub = sublistas xs

--- 4.5 Comprobación de subconjunto
--- Verifica si la primera lista es subconjunto de la segunda
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto [] _ = True
subconjunto (x:xs) ys = elem x ys && subconjunto xs ys

--- 4.6 Comprobación de la igualdad de conjuntos
--- Verifica si dos listas representan el mismo conjunto
igual_conjunto :: Eq a => [a] -> [a] -> Bool
igual_conjunto xs ys = subconjunto xs ys && subconjunto ys xs

--- 4.7 Permutaciones
--- Obtiene todas las permutaciones de una lista
permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = [a:p | a <- xs, p <- permutaciones (xs \\ [a])]

--- 4.8 Combinaciones
--- Obtiene todas las combinaciones n-arias de una lista
combinaciones :: Int -> [a] -> [[a]]
combinaciones n xs = [ys | ys <- sublistas xs, length ys == n]

--- 4.9 El problema de las reinas
--- Representa un tablero como una lista de posiciones de fila para cada columna
type Tablero = [Int]

--- Verifica si una reina no ataca a las demás
noAtaca :: Int -> Tablero -> Int -> Bool
noAtaca _ [] _ = True
noAtaca r (a:rs) distH = abs(r-a) /= distH && noAtaca r rs (distH+1)

--- Resuelve el problema de las N reinas
reinas :: Int -> [Tablero]
reinas n = reinasAux n
  where 
    reinasAux 0 = [[]]
    reinasAux m = [r:rs | rs <- reinasAux (m-1),
                          r <- ([1..n] \\ rs),
                          noAtaca r rs 1]

--- 4.10 Números de Hamming
--- Función auxiliar para mezclar tres listas ordenadas
mezcla3 :: Ord a => [a] -> [a] -> [a] -> [a]
mezcla3 xs ys zs = mezcla (mezcla xs ys) zs

--- Función auxiliar para mezclar dos listas ordenadas
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (x:xs) (y:ys)
  | x < y = x : mezcla xs (y:ys)
  | x > y = y : mezcla (x:xs) ys
  | otherwise = x : mezcla xs ys  -- x == y, tomamos solo uno

--- Genera la sucesión de números de Hamming
hamming :: [Int]
hamming = 1 : mezcla3 [2*i | i <- hamming]
                      [3*i | i <- hamming]
                      [5*i | i <- hamming]
