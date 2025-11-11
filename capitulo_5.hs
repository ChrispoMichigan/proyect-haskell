--- 5.1 Transformación entre euros y pesetas
--- Constante de cambio: 1 euro = 166.386 pesetas
cambioEuro :: Double
cambioEuro = 166.386

--- Función para convertir euros a pesetas
eurosPesetas :: Double -> Double
eurosPesetas euros = euros * cambioEuro

--- Función para convertir pesetas a euros
pesetasEuros :: Double -> Double
pesetasEuros pesetas = pesetas / cambioEuro

--- 5.2 Cuadrado
--- Calcula el cuadrado de un número entero
cuadrado :: Integer -> Integer
cuadrado x = x * x

--- 5.3 Valor absoluto
--- Calcula el valor absoluto de un número (redefinición de abs)
n_abs :: Integer -> Integer
n_abs x = if x > 0 then x else (-x)

--- 5.4 Potencia
--- Calcula x elevado a la potencia y (x^y)
potencia :: Integer -> Integer -> Integer
potencia x 0 = 1
potencia x n | n > 0 = x * potencia x (n-1)

--- 5.5 Regiones en el plano
--- Calcula el número máximo de regiones generadas con n líneas en el plano
regiones :: Integer -> Integer
regiones 0 = 1
regiones n | n > 0 = regiones (n-1) + n
