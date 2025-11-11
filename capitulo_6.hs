--- 6.1 Modelización de un juego de cartas

--- Definición del tipo Palo
data Palo = Picas | Corazones | Diamantes | Treboles
  deriving (Eq, Show)

--- Definición del tipo Color
data Color = Rojo | Negro
  deriving Show

--- Función para obtener el color de un palo
color :: Palo -> Color
color Picas = Negro
color Corazones = Rojo
color Diamantes = Rojo
color Treboles = Negro

--- Definición del tipo Valor para las cartas
data Valor = Numerico Int | Sota | Reina | Rey | As
  deriving (Eq, Show)

--- Función para comparar valores de cartas (mayor valor)
mayor :: Valor -> Valor -> Bool
mayor _ As = False
mayor As _ = True
mayor _ Rey = False
mayor Rey _ = True
mayor _ Reina = False
mayor Reina _ = True
mayor _ Sota = False
mayor Sota _ = True
mayor (Numerico m) (Numerico n) = m > n

--- Definición del tipo Carta
data Carta = Carta Valor Palo
  deriving (Eq, Show)

--- Función para obtener el valor de una carta
valor :: Carta -> Valor
valor (Carta v p) = v

--- Función para obtener el palo de una carta
palo :: Carta -> Palo
palo (Carta v p) = p

--- Definición alternativa de Carta con sintaxis de registro
data Carta1 = Carta1 {valor1 :: Valor, palo1 :: Palo}
  deriving Show

--- Función para determinar si una carta gana a otra
ganaCarta :: Palo -> Carta -> Carta -> Bool
ganaCarta triunfo c c'
  | palo c == palo c' = mayor (valor c) (valor c')
  | palo c == triunfo = True
  | otherwise = False

--- Definición del tipo Mano
data Mano = Vacia | Anade Carta Mano
  deriving (Eq, Show)

--- Función para determinar si una mano gana a una carta
ganaMano :: Palo -> Mano -> Carta -> Bool
ganaMano triunfo Vacia c' = False
ganaMano triunfo (Anade c m) c' = ganaCarta triunfo c c' || ganaMano triunfo m c'

--- Función para elegir la mejor carta de una mano
eligeCarta :: Palo -> Carta -> Mano -> Carta
eligeCarta triunfo c1 (Anade c Vacia) = c
eligeCarta triunfo c1 (Anade c resto)
  | palo c == palo c1 && palo c' /= palo c1 = c
  | palo c /= palo c1 && palo c' == palo c1 = c'
  | ganaCarta triunfo c c1 && not (ganaCarta triunfo c' c1) = c
  | ganaCarta triunfo c' c1 && not (ganaCarta triunfo c c1) = c'
  | mayor (valor c) (valor c') = c'
  | otherwise = c
  where
    c' = eligeCarta triunfo c1 resto

--- 6.2 Simplificación de definiciones

--- Función simplificada para verificar si un número es grande
esGrande :: Integer -> Bool
esGrande n = n > 9999

--- 6.3 Definición del tipo lista

--- Definición del tipo Lista personalizado
data Lista a = VaciaL | AnadeL a (Lista a)
  deriving (Show, Eq)

--- Definición de Mano usando Lista
type Mano' = Lista Carta

--- Función para verificar si una lista está vacía
esVaciaL :: Lista a -> Bool
esVaciaL VaciaL = True
esVaciaL (AnadeL x lista) = False

--- Función para obtener el primer elemento de una lista
primeroL :: Lista a -> a
primeroL (AnadeL x lista) = x

--- Versión con manejo de error
primeroL' :: Lista a -> a
primeroL' VaciaL = error "la lista es vacia"
primeroL' (AnadeL x lista) = x

--- Función para obtener el último elemento de una lista
ultimoL :: Lista a -> a
ultimoL (AnadeL x VaciaL) = x
ultimoL (AnadeL x lista) = ultimoL lista

--- Funciones equivalentes usando listas estándar

--- Verifica si una lista estándar está vacía
esVacia2 :: [a] -> Bool
esVacia2 [] = True
esVacia2 (x:lista) = False

--- Obtiene el primer elemento de una lista estándar
primero2 :: [a] -> a
primero2 (x:lista) = x

--- Obtiene el último elemento de una lista estándar
ultimo2 :: [a] -> a
ultimo2 [x] = x
ultimo2 (x:lista) = ultimo2 lista

--- Suma los elementos de una lista
suma :: Num a => [a] -> a
suma [] = 0
suma (x:xs) = x + suma xs

--- 6.4 Concatenación de dos listas

--- Concatena dos listas (redefinición de ++)
conc :: [a] -> [a] -> [a]
conc [] ys = ys
conc (x:xs) ys = x : (conc xs ys)

--- 6.5 Inversa de una lista

--- Invierte una lista (redefinición de reverse)
inversa :: [a] -> [a]
inversa [] = []
inversa (x:xs) = inversa xs ++ [x]
