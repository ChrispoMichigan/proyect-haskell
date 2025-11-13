import System.IO
import System.Directory
import Data.List (sort)
-- stack install random
-- stack install QuickCheck

-- import System.Random
-- import Test.QuickCheck (para generadores)

--- 9.1 Copia de ficheros
--- Copia el contenido de un fichero a otro
copiaFichero :: FilePath -> FilePath -> IO ()
copiaFichero f1 f2 = do
  contenido <- readFile f1
  writeFile f2 contenido

--- 9.2 Acción y escritura

--- Ejecuta una acción IO y muestra su resultado
escribe :: Show a => IO a -> IO ()
escribe io = do
  resultado <- io
  print resultado

--- Ejecuta una acción dos veces y devuelve ambos resultados
dosVeces :: Monad m => m b -> m (b,b)
dosVeces io = do
  a <- io
  b <- io
  return (a,b)

--- 9.3 Muestra de valores generados
--- (Requiere QuickCheck - comentado para compatibilidad)

{-
--- Muestra 5 valores generados por un generador de QuickCheck
muestra :: Show a => Gen a -> IO ()
muestra gen = do
  sequence_ [do rnd <- newStdGen
                print (generate 5 rnd gen)
            | i <- [1..5]]
-}

--- 9.4 Generación de listas
--- (Requiere QuickCheck - comentado para compatibilidad)

{-
--- Genera una lista de n elementos usando el generador dado
listaDe :: Int -> Gen a -> Gen [a]
listaDe n g = sequence [g | i <- [1..n]]

--- Genera pares de listas de igual longitud
paresDeIgualLongitud :: Gen a -> Gen ([a],[a])
paresDeIgualLongitud gen = do
  n <- arbitrary
  xs <- listaDe (abs n) gen
  ys <- listaDe (abs n) gen
  return (xs,ys)

--- Propiedad para verificar que listaDe genera listas de longitud correcta
prop_listaDe :: Int -> Property
prop_listaDe n = 
  forAll (listaDe (abs n) (arbitrary :: Gen Int)) $ \xs ->
    length xs == (abs n)
-}

--- 9.5 Mayorías parlamentarias

--- Tipo de datos para representar partidos políticos
data Partido = P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8
  deriving (Eq, Ord, Show)

--- Tipo para representar número de escaños
type Escanos = Integer

--- Tipo para representar tablas como listas de pares
type Tabla a b = [(a,b)]

--- Tipo para representar una asamblea parlamentaria
type Asamblea = Tabla Partido Escanos

--- Obtiene la lista de partidos en una asamblea
partidos :: Asamblea -> [Partido]
partidos a = [p | (p,_) <- a]

--- Calcula el número total de escaños en una asamblea
escanos :: Asamblea -> Integer
escanos a = sum [e | (_,e) <- a]

--- 9.6 Copia de respaldo

--- Copia un fichero solo si existe
copiaFicheroSegura :: FilePath -> FilePath -> IO ()
copiaFicheroSegura f1 f2 = do
  existe <- doesFileExist f1
  if existe
    then copyFile f1 f2
    else return ()

--- Crea una copia de respaldo del directorio actual
respaldo :: IO ()
respaldo = do
  ficheros <- getDirectoryContents "."
  print ficheros
  createDirectory "/tmp/respaldo"
  sequence_ [copiaFicheroSegura fichero ("/tmp/respaldo/" ++ fichero)
            | fichero <- ficheros]

--- 9.7 Ordenación de fichero

--- Ordena las líneas de un fichero y las guarda en otro
ordenaFichero :: FilePath -> FilePath -> IO ()
ordenaFichero f1 f2 = do
  s <- readFile f1
  writeFile f2 (unlines (sort (lines s)))

--- 9.8 Escritura de tablas

--- Escribe una lista de strings como tabla numerada
escribeTabla :: [String] -> IO ()
escribeTabla xs = 
  sequence_ [putStrLn (show i ++ ": " ++ x)
            | (x,i) <- xs `zip` [1..]]

--- 9.9 Juego interactivo para adivinar un número

--- Juego 1: La máquina adivina un número pensado por el humano
juego1 :: IO ()
juego1 = do
  putStrLn "Piensa un numero entre el 1 y el 100."
  adivina 1 100
  putStrLn "Fin del juego"

--- Función auxiliar para el juego 1 - estrategia de búsqueda binaria
adivina :: Int -> Int -> IO ()
adivina a b = do
  putStr ("Es " ++ show conjetura ++ "? [mayor/menor/exacto] ")
  s <- getLine
  case s of
    "mayor" -> adivina (conjetura+1) b
    "menor" -> adivina a (conjetura-1)
    "exacto" -> return ()
    _ -> adivina a b
  where
    conjetura = (a+b) `div` 2

--- Juego 2: El humano adivina un número generado por la máquina
--- (Requiere System.Random - comentado para compatibilidad)
{-
juego2 :: IO ()
juego2 = do
  hSetBuffering stdout NoBuffering
  n <- randomRIO (1::Int, 100)
  putStrLn "Tienes que adivinar un numero entre 1 y 100"
  adivina' n

--- Función auxiliar para el juego 2
adivina' :: Int -> IO ()
adivina' n = do
  putStr "Escribe un numero: "
  c <- getLine
  let x = read c
  case (compare x n) of
    LT -> do putStrLn " es bajo."
             adivina' n
    GT -> do putStrLn " es alto."
             adivina' n
    EQ -> do putStrLn " Exactamente"
-}
