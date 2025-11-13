import Data.Char (isUpper, isLower, isDigit, ord)

--- 11.1 Analizadores mediante listas de comprensión

--- Tipo para analizadores (renombrado para evitar conflictos)
type Parser a = String -> [(a, String)]

--- Analizador que siempre tiene éxito sin consumir entrada
exito :: a -> Parser a
exito x = \c -> [(x, c)]

--- Analizador que no consume entrada y devuelve ()
epsilon :: Parser ()
epsilon = exito ()

--- Analizador que siempre falla
fallo :: Parser a
fallo = \c -> []

--- Analizador que reconoce un carácter específico
rChar :: Char -> Parser Char
rChar c = \s -> case s of
  [] -> []
  x:xs -> if c == x then [(x, xs)] else []

--- Analizador que reconoce caracteres que satisfacen un predicado
rSat :: (Char -> Bool) -> Parser Char
rSat p = \s -> case s of
  [] -> []
  x:xs -> if p x then [(x, xs)] else []

--- Versión de rChar usando rSat
rChar' :: Char -> Parser Char
rChar' x = rSat (== x)

--- Combinador de elección (alternativa)
infixl 5 -+-
(-+-) :: Parser a -> Parser a -> Parser a
p1 -+- p2 = \s -> p1 s ++ p2 s

--- Combinador de aplicación de función
infixl 6 >=>
(>=>) :: Parser a -> (a -> b) -> Parser b
p >=> f = \s -> [(f x, s1) | (x, s1) <- p s]

--- Combinador de secuencia
infixl 7 &><
(&><) :: Parser a -> Parser b -> Parser (a, b)
p1 &>< p2 = \s -> [((x1, x2), s2) | (x1, s1) <- p1 s,
                                    (x2, s2) <- p2 s1]

--- Combinador de secuencia con función
infixr 6 >>>
(>>>) :: Parser a -> (a -> Parser b) -> Parser b
p >>> f = \s -> [(y, s2) | (x, s1) <- p s,
                           (y, s2) <- (f x) s1]

--- Analizador para dos letras mayúsculas
rAB :: Parser (Char, Char)
rAB = rSat isUpper >>> \x ->
      rSat isUpper >>> \y ->
      exito (x, y)

--- Analizador para dos letras mayúsculas iguales
rDosIguales :: Parser (Char, Char)
rDosIguales = rSat isUpper >>> \x ->
              rChar x >>> \y ->
              exito (x, y)

--- Analizador que aplica otro una o más veces
rep1 :: Parser a -> Parser [a]
rep1 p = varios -+- uno
  where
    varios = p >>> \x ->
             rep1 p >>> \xs ->
             exito (x:xs)
    uno = p >>> \x ->
          exito [x]

--- Analizador que aplica otro cero o más veces
rep0 :: Parser a -> Parser [a]
rep0 p = rep1 p -+-
         (epsilon >>> \() ->
          exito [])

--- Funciones auxiliares para números
chrAInteger :: Char -> Integer
chrAInteger c = toInteger (ord c - ord '0')

aInteger :: [Char] -> Integer
aInteger = foldl1 (\x y -> 10*x + y) . map chrAInteger

--- Analizador para números naturales (múltiples resultados)
rNumNatural' :: Parser Integer
rNumNatural' = rep1 (rSat isDigit) >>> \cs ->
               exito (aInteger cs)

--- Función para obtener el primer resultado
primero :: Parser a -> Parser a
primero p = \s -> case p s of
  [] -> []
  x:_ -> [x]

--- Analizador para números naturales (un resultado)
rNumNatural :: Parser Integer
rNumNatural = primero rNumNatural'

--- Combinador de valor por defecto
infix 8 ?
(?) :: Parser a -> a -> Parser a
p ? ifNone = p -+-
             (epsilon >>> \() ->
              exito ifNone)

--- Función auxiliar para convertir signo y número
aNúmero :: Char -> Integer -> Integer
aNúmero '+' n = n
aNúmero '-' n = -n

--- Analizador para números enteros
rNumEntero :: Parser Integer
rNumEntero = (rChar '+' -+- rChar '-') ? '+' >>> \s ->
             rNumNatural >>> \n ->
             exito (aNúmero s n)

--- Analizador para listas de enteros (versión simple)
rListaEnteros' :: Parser [Integer]
rListaEnteros' = 
  rChar '[' >>> \_ ->
  rElems >>> \es ->
  rChar ']' >>> \_ ->
  exito es
  where
    rElems = rNumEntero >>> \n ->
             rep0 rComaEntero >>> \ns ->
             exito (n:ns)
    rComaEntero = rChar ',' >>> \_ ->
                  rNumEntero >>> \n ->
                  exito n

--- Analizador para cadenas específicas
rString :: String -> Parser String
rString [] = exito ""
rString (c:cs) = rChar c >>> \_ ->
                 rString cs >>> \rs ->
                 exito (c:rs)

--- Función para detectar espacios en blanco
esEspacio :: Char -> Bool
esEspacio = (`elem` " \n\t")

--- Analizador que ignora espacios en blanco iniciales
rLex :: Parser a -> Parser a
rLex p = rep0 (rSat esEspacio) >>> \_ ->
         p

--- Analizador para tokens (cadenas sin espacios iniciales)
rToken :: String -> Parser String
rToken s = rLex (rString s)

--- Analizador para números naturales sin espacios iniciales
rNat :: Parser Integer
rNat = rLex rNumNatural

--- Analizador para números enteros sin espacios iniciales
rInteger :: Parser Integer
rInteger = rLex rNumEntero

--- Analizador para listas de enteros con espacios
rListaEnteros :: Parser [Integer]
rListaEnteros = 
  rToken "[" >>> \_ ->
  rElems >>> \es ->
  rToken "]" >>> \_ ->
  exito es
  where
    rElems = (rInteger >>> \n ->
              rep0 rComaEntero >>> \ns ->
              exito (n:ns)) -+-
             (epsilon >>> \() ->
              exito [])
    rComaEntero = rToken "," >>> \_ ->
                  rInteger >>> \n ->
                  exito n

--- Tipo de datos para árboles binarios
data Arbol a = Hoja a | Nodo (Arbol a) (Arbol a)

--- Ejemplo de árbol
ejArbol :: Arbol Int
ejArbol = Nodo (Hoja 1) (Nodo (Hoja 2) (Hoja 3))

--- Función auxiliar para mostrar árboles
muestraArbol' :: (Show a) => Arbol a -> ShowS
muestraArbol' (Hoja x) = shows x
muestraArbol' (Nodo l r) = ('<':) . 
                          muestraArbol' l . 
                          ('|':) . 
                          muestraArbol' r . 
                          ('>':)

--- Función para mostrar árboles
muestraArbol :: (Show a) => Arbol a -> String
muestraArbol x = muestraArbol' x ""

--- Instancia Show para árboles
instance Show a => Show (Arbol a) where
  show = muestraArbol

--- Analizador para leer árboles
leeArbol :: (Read a) => ReadS (Arbol a)
leeArbol ('<':s) = [(Nodo l r, u) | (l, '|':t) <- leeArbol s,
                                    (r, '>':u) <- leeArbol t]
leeArbol s = [(Hoja x, t) | (x, t) <- reads s]

--- Instancia Read para árboles
instance Read a => Read (Arbol a) where
  readsPrec _ s = leeArbol s
