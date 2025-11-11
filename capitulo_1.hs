--- Factorial 1.1
factorial n = if n == 0 then 1 else n * factorial (n - 1)

--- Número de combinaciones 1.2
combinaciones n m = factorial n / (factorial m * factorial (n - m))

--- Comprobación de número impar 1.3
esImpar = odd

--- Cuadrado 1.4
cuadrado n = n * n

--- Suma de cuadrados
--- Suma los cuadrados de una lista
suma_cuadrados_1 l =  sum ( map cuadrado l)

--- 1.6. Raices de ecuaciones de segundo grado
--- Sacar la raices de una ecuación de segundo grado
raices a b c = [ (-b+sqrt(b*b-4*a*c))/(2*a), (-b-sqrt(b*b-4*a*c))/(2*a) ]

--- 1.7 Valor absoluto
nAbs x = if x>0 then x else (-x)
--- 1.8 Signo
nsigno x
  | x < 0 = - 1
  | x > 0 = 1
  | otherwise = 0
--- 1.9 Conjunción
conjuncion x y = x && y
--- 1.10 Anterior de un número natural
anterior n = if n <= 0 then 0 else n - 1
anterior_2 n | n>0 = n-1