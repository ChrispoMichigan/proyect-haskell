--- 2.1 Casi igual
--- Operador que verifica si dos números flotantes son casi iguales
infix 4 ~=
(~=) :: Float -> Float -> Bool
x ~= y = abs(x-y) < 0.0001

--- 2.2 Siguiente de un número
--- Calcula el siguiente número entero
siguiente :: Integer -> Integer
siguiente x = x + 1

--- 2.3 Doble
--- Calcula el doble de un número
doble :: Num a => a -> a
doble x = 2 * x

--- 2.4 Mitad
--- Calcula la mitad de un número
mitad :: Double -> Double
mitad x = x / 2

--- 2.5 Inverso
--- Calcula el inverso de un número (1/x)
inverso :: Double -> Double
inverso x = 1 / x

--- 2.6 Potencia de dos
--- Calcula 2 elevado a la potencia x
dosElevadoA :: Int -> Int
dosElevadoA x = 2^x

--- 2.7 Reconocimiento de números positivos
--- Verifica si un número es positivo
esPositivo :: Int -> Bool
esPositivo x = x > 0

--- 2.8 Aplicación de una función a los elementos de una lista
--- Aplica una función a cada elemento de una lista (redefinición de map)
n_map :: (a -> b) -> [a] -> [b]
n_map f [] = []
n_map f (x:xs) = f x : n_map f xs

--- 2.9 Filtrado mediante una propiedad
--- Filtra elementos de una lista según una condición (redefinición de filter)
n_filter :: (a -> Bool) -> [a] -> [a]
n_filter p [] = []
n_filter p (x:xs) 
  | p x = x : n_filter p xs
  | otherwise = n_filter p xs

--- 2.10 Suma de los elementos de una lista
--- Suma todos los elementos de una lista (redefinición de sum)
n_sum :: Num a => [a] -> a
n_sum [] = 0
n_sum (x:xs) = x + n_sum xs
