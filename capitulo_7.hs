--- 7.1 La función máximo
--- Calcula el máximo de dos números enteros
maxI :: Integer -> Integer -> Integer
maxI x y | x >= y = x
         | otherwise = y

--- 7.2 Suma de cuadrados
--- Calcula la suma de cuadrados de números de 1 a n
sumaCuadrados :: Integer -> Integer
sumaCuadrados 0 = 0
sumaCuadrados n | n > 0 = sumaCuadrados (n-1) + n*n

--- 7.3 Potencia
--- Calcula x elevado a la potencia n
potencia :: Integer -> Integer -> Integer
potencia x 0 = 1
potencia x n | n > 0 = x * potencia x (n-1)

--- 7.4 Las torres de Hanoi
--- Calcula el número de movimientos necesarios para n discos
hanoi :: Integer -> Integer
hanoi 1 = 1
hanoi n | n > 1 = 1 + 2 * (hanoi (n-1))

--- 7.5 Los números de Fibonacci
--- Calcula el n-ésimo número de Fibonacci
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n | n > 1 = fib (n-1) + fib (n-2)

--- 7.6 Divisores
--- Verifica si a divide a b
divide :: Integer -> Integer -> Bool
divide a b = b `mod` a == 0

--- 7.7 Multiplicación de una lista de números
--- Multiplica todos los elementos de una lista (equivalente a product)
multiplica :: Num a => [a] -> a
multiplica [] = 1
multiplica (x:xs) = x * multiplica xs

--- 7.8 Eliminación de elementos duplicados
--- Verifica si una lista contiene elementos duplicados
duplicados :: Eq a => [a] -> Bool
duplicados [] = False
duplicados (x:xs) = elem x xs || duplicados xs

--- 7.9 Fechas
--- Tipo de datos para representar los meses del año
data Mes = Enero
         | Febrero
         | Marzo
         | Abril
         | Mayo
         | Junio
         | Julio
         | Agosto
         | Septiembre
         | Octubre
         | Noviembre
         | Diciembre
  deriving (Eq, Show)

--- Función para verificar si un número es divisible por otro
divisible :: Int -> Int -> Bool
divisible x y = x `rem` y == 0
