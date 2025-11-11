--- 3.1 Relación de igualdad entre listas
--- Verifica si dos listas son iguales (equivalente a ==)
igualLista :: Eq a => [a] -> [a] -> Bool
igualLista [] [] = True
igualLista (x:xs) (y:ys) = x == y && igualLista xs ys
igualLista _ _ = False

--- 3.2 Concatenación de listas
--- Concatena dos listas (equivalente a ++)
conc :: [a] -> [a] -> [a]
conc [] ys = ys
conc (x:xs) ys = x : (conc xs ys)

--- 3.3 Concatenación de una lista de listas
--- Concatena todas las listas dentro de una lista (redefinición de concat)
n_concat :: [[a]] -> [a]
n_concat [] = []
n_concat (xs:xss) = xs ++ n_concat xss

--- 3.4 Cabeza de una lista
--- Obtiene el primer elemento de una lista (redefinición de head)
n_head :: [a] -> a
n_head (x:_) = x

--- 3.5 Resto de una lista
--- Obtiene todos los elementos excepto el primero (redefinición de tail)
n_tail :: [a] -> [a]
n_tail (_:xs) = xs

--- 3.6 Último elemento
--- Obtiene el último elemento de una lista (redefinición de last)
n_last :: [a] -> a
n_last [x] = x
n_last (_:xs) = n_last xs

--- 3.7 Lista sin el último elemento
--- Obtiene la lista sin el último elemento (redefinición de init)
n_init :: [a] -> [a]
n_init [x] = []
n_init (x:xs) = x : n_init xs

--- 3.8 Segmento inicial
--- Toma los primeros n elementos de una lista (redefinición de take)
n_take :: Int -> [a] -> [a]
n_take n _ | n <= 0 = []
n_take _ [] = []
n_take n (x:xs) = x : n_take (n-1) xs

--- 3.9 Segmento inicial filtrado
--- Toma elementos iniciales que cumplen una condición (redefinición de takeWhile)
n_takeWhile :: (a -> Bool) -> [a] -> [a]
n_takeWhile p [] = []
n_takeWhile p (x:xs)
  | p x = x : n_takeWhile p xs
  | otherwise = []

--- 3.10 Segmento final
--- Elimina los primeros n elementos de una lista (redefinición de drop)
n_drop :: Int -> [a] -> [a]
n_drop n xs | n <= 0 = xs
n_drop _ [] = []
n_drop n (_:xs) = n_drop (n-1) xs
