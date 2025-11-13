
-- ! Recordarme ejecutar con
--- ghci capitulo_13.hs
-- ? luego main
-- * Por que si no, no jala

-- ============================================================================
-- Capítulo 13: Juegos - Tres en raya
-- ============================================================================
-- Este capítulo implementa un juego de tres en raya (tic-tac-toe) donde 
-- la computadora juega contra el humano usando la estrategia minimax.
--
-- Las posiciones del tablero se numeran:
-- 1|2|3
-- -+-+-
-- 4|5|6
-- -+-+-
-- 7|8|9
-- ============================================================================

import Data.List
import System.IO

-- ============================================================================
-- Ejercicio 13.1.1: Constante profundidadDeBusqueda
-- ============================================================================
-- profundidadDeBusqueda es el máximo nivel de profundidad del árbol 
-- de análisis del juego. Por defecto es 6.
profundidadDeBusqueda :: Int
profundidadDeBusqueda = 6

-- ============================================================================
-- Ejercicios 13.1.2-13.1.5: Tipos de datos básicos
-- ============================================================================

-- Tipo para representar una posición del tablero (1-9)
type Posicion = Int

-- Tipo para representar listas de posiciones
type Posiciones = [Posicion]

-- Tipo para representar el tablero
-- Tab xs os donde xs son posiciones de X y os son posiciones de O
data Tablero = Tab Posiciones Posiciones
    deriving Show

-- Tablero inicial vacío
tableroInicial :: Tablero
tableroInicial = Tab [] []

-- ============================================================================
-- Ejercicios 13.1.6-13.1.11: Funciones básicas del tablero
-- ============================================================================

-- turnoDeX t: verifica si le toca mover al jugador X
-- X comienza, por lo que le toca cuando ambas listas tienen igual longitud
turnoDeX :: Tablero -> Bool
turnoDeX (Tab xs os) = 
    length xs == length os

-- pone t p: coloca una ficha en la posición p del tablero t
-- La ficha es del jugador al que le corresponde el turno
pone :: Tablero -> Posicion -> Tablero
pone (Tab xs os) p =
    if turnoDeX (Tab xs os)
        then Tab (p:xs) os
        else Tab xs (p:os)

-- completo t: verifica si el tablero t está completo (9 fichas colocadas)
completo :: Tablero -> Bool
completo (Tab xs os) =
    length xs + length os == 9

-- subconjunto s1 s2: verifica si s1 es subconjunto de s2
subconjunto :: Posiciones -> Posiciones -> Bool
subconjunto s1 s2 =
    all (`elem` s2) s1

-- tieneLinea ps: verifica si las posiciones ps contienen una línea completa
-- Las líneas pueden ser horizontales, verticales o diagonales
tieneLinea :: Posiciones -> Bool
tieneLinea ps =
    subconjunto [1,2,3] ps ||  -- fila superior
    subconjunto [4,5,6] ps ||  -- fila media
    subconjunto [7,8,9] ps ||  -- fila inferior
    subconjunto [1,4,7] ps ||  -- columna izquierda
    subconjunto [2,5,8] ps ||  -- columna central
    subconjunto [3,6,9] ps ||  -- columna derecha
    subconjunto [1,5,9] ps ||  -- diagonal principal
    subconjunto [3,5,7] ps     -- diagonal secundaria

-- tieneGanador t: verifica si el tablero t tiene un ganador
tieneGanador :: Tablero -> Bool
tieneGanador (Tab xs os) =
    tieneLinea xs || tieneLinea os

-- ============================================================================
-- Ejercicios 13.1.12-13.1.13: Tipo de datos Árbol
-- ============================================================================

-- Tipo de datos para representar árboles con lista de hijos
data Arbol a = Nodo a [Arbol a]

-- muestraArbol: convierte un árbol en una cadena para visualización
muestraArbol :: Show t => Arbol t -> String
muestraArbol (Nodo x xs) =
    show x ++ '\n' : (unlines . map (" "++) . concatMap (lines . show)) xs

-- Hacer Arbol instancia de Show
instance Show a => Show (Arbol a) where
    show = muestraArbol

-- ============================================================================
-- Ejercicios 13.1.14-13.1.16: Construcción del árbol de juego
-- ============================================================================

-- posicionesLibres t: lista de posiciones libres en el tablero t
posicionesLibres :: Tablero -> Posiciones
posicionesLibres (Tab xs os) =
    [1..9] \\ (xs ++ os)

-- siguientesTableros t: lista de tableros obtenidos colocando una pieza
-- en cada posición libre de t
siguientesTableros :: Tablero -> [Tablero]
siguientesTableros t =
    if tieneGanador t
        then []
        else map (pone t) (posicionesLibres t)

-- construyeArbol t: árbol de juego correspondiente al tablero t
construyeArbol :: Tablero -> Arbol Tablero
construyeArbol t =
    Nodo t (map construyeArbol (siguientesTableros t))

-- ============================================================================
-- Ejercicios 13.1.17-13.1.23: Algoritmo minimax
-- ============================================================================

-- Tipo para representar valores de tableros
type Valor = Int

-- valores vts: extrae los valores de una lista de árboles valorados
valores :: [Arbol (Valor,Tablero)] -> [Valor]
valores vts =
    [v | Nodo (v,_) _ <- vts]

-- maximiza: asigna valores máximos a los nodos del árbol
-- El jugador maximizador busca ganar (+1) o evitar perder (-1)
maximiza :: Arbol Tablero -> Arbol (Valor,Tablero)
maximiza (Nodo t []) = 
    Nodo (if tieneGanador t then -1 else 0, t) []
maximiza (Nodo t ts) = 
    Nodo (maximum (valores vts), t) vts
    where vts = map minimiza ts

-- minimiza: asigna valores mínimos a los nodos del árbol  
-- El jugador minimizador busca que el oponente no gane
minimiza :: Arbol Tablero -> Arbol (Valor,Tablero)
minimiza (Nodo t []) = 
    Nodo (if tieneGanador t then 1 else 0, t) []
minimiza (Nodo t ts) = 
    Nodo (minimum (valores vts), t) vts
    where vts = map maximiza ts

-- poda n a: poda el árbol a partir de la profundidad n
poda :: Int -> Arbol a -> Arbol a
poda n (Nodo x xs) =
    Nodo x (if n == 0
                then []
                else map (poda (n-1)) xs)

-- selecciona: elige el tablero del primer hijo cuyo valor coincide con la raíz
selecciona :: Arbol (Valor,Tablero) -> Tablero
selecciona (Nodo (v,_) ts) =
    case [t | Nodo (v',t) _ <- ts, v' == v] of
        (t:_) -> t
        []    -> error "No hay movimiento válido"

-- mejorMovimiento t: calcula el mejor movimiento desde el tablero t
mejorMovimiento :: Tablero -> Tablero
mejorMovimiento =
    selecciona . maximiza . poda profundidadDeBusqueda . construyeArbol

-- ============================================================================
-- Ejercicios 13.1.24-13.1.26: Dibujo del tablero
-- ============================================================================

-- muestraPosicion t p: contenido de la posición p en el tablero t
-- Devuelve "X", "O" o el número de posición si está libre
muestraPosicion :: Tablero -> Posicion -> String
muestraPosicion (Tab xs os) p
    | p `elem` xs = "X"
    | p `elem` os = "O"  
    | otherwise   = show p

-- muestraLinea t ps: cadena con el contenido de las posiciones ps 
-- separadas por barras verticales
muestraLinea :: Tablero -> [Posicion] -> String
muestraLinea t =
    concat . intersperse "|" . map (muestraPosicion t)

-- muestraTablero t: representación visual completa del tablero t
muestraTablero :: Tablero -> String
muestraTablero t =
    muestraLinea t [1..3] ++ "\n-+-+-\n" ++
    muestraLinea t [4..6] ++ "\n-+-+-\n" ++
    muestraLinea t [7..9]

-- ============================================================================
-- Ejercicios 13.1.27-13.1.29: Control del juego
-- ============================================================================

-- main: función principal que controla el juego
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering        -- Escritura inmediata
    putStrLn "Tres en raya"                -- Nombre del juego
    putStrLn (muestraTablero tableroInicial) -- Tablero inicial
    putStr "Comienza el juego? (s/n) "     -- Pregunta al usuario
    l <- getLine                           -- Lee respuesta
    case l of
        (c:_) | c `elem` ['s','S'] -> humano tableroInicial    -- Humano comienza
        _                          -> computadora tableroInicial -- Computadora comienza

-- humano t: maneja el turno del jugador humano
humano :: Tablero -> IO ()
humano t = do
    putStr "\nIndica el lugar donde colocar la ficha: "
    l <- getLine
    let t' = pone t (read l :: Posicion)
    putStrLn (muestraTablero t')
    if tieneGanador t'
        then putStrLn "Has ganado."
        else if completo t'
                then putStrLn "Empate."
                else computadora t'

-- computadora t: maneja el turno de la computadora
computadora :: Tablero -> IO ()
computadora t = do
    putStrLn "\nMi jugada:"
    let t' = mejorMovimiento t
    putStrLn (muestraTablero t')
    if tieneGanador t'
        then putStrLn "He ganado."
        else if completo t'
                then putStrLn "Empate."
                else humano t'

-- ============================================================================
-- Ejemplos de uso:
-- ============================================================================
-- *Main> main
-- Tres en raya
-- 1|2|3
-- -+-+-
-- 4|5|6
-- -+-+-
-- 7|8|9
-- Comienza el juego? (s/n) s
-- 
-- Indica el lugar donde colocar la ficha: 5
-- 1|2|3
-- -+-+-
-- 4|X|6
-- -+-+-
-- 7|8|9
--
-- Mi jugada:
-- O|2|3
-- -+-+-
-- 4|X|6
-- -+-+-
-- 7|8|9
-- ============================================================================
