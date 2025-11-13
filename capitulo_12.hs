--- 12.1 Búsqueda en profundidad en grafos

--- Tipo de datos para grafos simples
data Grafo1 v = G1 [v] (v -> [v])

--- Ejemplo de grafo simple
ej_grafo1 :: Grafo1 Int
ej_grafo1 = G1 [1..5] suc
  where 
    suc 1 = [2,3]
    suc 2 = [4]
    suc 3 = [4]
    suc _ = []

--- Busca un camino desde u hasta v
camino1 :: Eq a => Grafo1 a -> a -> a -> [a]
camino1 g u v = 
    case caminosDesde1 g u (== v) [] of
        (c:_) -> c
        []    -> error "No hay camino disponible"

--- Encuentra todos los caminos desde origen hasta destinos que cumplan test
caminosDesde1 :: Eq a => Grafo1 a -> a -> (a -> Bool) -> [a] -> [[a]]
caminosDesde1 g o te vis
  | te o = [o:vis]
  | otherwise = concat [caminosDesde1 g o' te (o:vis) | 
                        o' <- suc o,
                        notElem o' vis]
  where G1 _ suc = g

--- 12.2 Búsqueda en profundidad en grafos con pesos

--- Tipo de datos para grafos con pesos
data Grafo2 v p = G2 [v] (v -> [(v,p)])

--- Ejemplo de grafo con pesos
ej_grafo2 :: Grafo2 Int Int
ej_grafo2 = G2 [1..4] suc
  where 
    suc 1 = [(2,30), (3,20), (4,40)]
    suc 2 = [(4,20)]
    suc 3 = [(4,50)]
    suc _ = []

--- Busca caminos con coste menor o igual a pt
caminos2 :: (Eq a, Num b, Ord b) => Grafo2 a b -> a -> a -> b -> [[a]]
caminos2 g u v pt = caminosDesde2 g u (\x _ -> x==v) (> pt) [] 0

--- Búsqueda en profundidad con control de peso
caminosDesde2 :: (Eq a, Num b) => Grafo2 a b -> a -> (a -> b -> Bool) -> 
                 (b -> Bool) -> [a] -> b -> [[a]]
caminosDesde2 g o te tr vis p
  | te o p = [o:vis]
  | otherwise = concat [caminosDesde2 g o' te tr (o:vis) np | 
                        (o',p') <- suc o,
                        notElem o' vis,
                        let np = p+p',
                        not(tr np)]
  where G2 _ suc = g

--- Calcula el coste de un camino
costeCamino :: (Num a, Eq b) => Grafo2 b a -> [b] -> a
costeCamino _ [_] = 0
costeCamino g (u:v:xs) = p + costeCamino g (v:xs)
  where 
    G2 _ suc = g
    Just p = lookup u (suc v)

--- Inserta un camino ordenado por coste
insertaCamino :: (Num a, Eq b, Ord a) => Grafo2 b a -> [b] -> [[b]] -> [[b]]
insertaCamino g c [] = [c]
insertaCamino g c (x:xs)
  | costeCamino g c <= costeCamino g x = c:x:xs
  | otherwise = x : insertaCamino g c xs

--- Ordena caminos por coste
ordenaCaminos :: (Ord a, Eq b, Num a) => Grafo2 b a -> [[b]] -> [[b]]
ordenaCaminos g = foldr (insertaCamino g) []

--- 12.3 La clase grafo con búsqueda en profundidad

--- Clase para grafos con búsqueda en profundidad
class Eq v => GrafoBusq v where
  vertices :: [v]
  suc3 :: v -> [v]
  camino3 :: v -> v -> [v]
  caminosDesde3 :: v -> (v -> Bool) -> [v] -> [[v]]
  tv :: v -> [v] -> Bool
  
  -- Métodos por defecto
  camino3 u v = 
    case caminosDesde3 u (== v) [] of
        (c:_) -> c
        []    -> error "No hay camino disponible"
  caminosDesde3 o te vis
    | te o = [o:vis]
    | otherwise = concat [caminosDesde3 o' te (o:vis) | 
                          o' <- suc3 o,
                          tv o' vis]
  tv = notElem

--- Tipo de datos para vértices del ejemplo
data Vertice = A | B | C | D | E 
  deriving (Show, Eq, Enum)

--- Instancia del grafo ejemplo
instance GrafoBusq Vertice where
  vertices = [A .. E]
  suc3 A = [B,C,D]
  suc3 B = [C]
  suc3 C = [A,D]
  suc3 D = [C]
  suc3 E = []

--- 12.4 La clase grafo con búsqueda con pesos

--- Tipo para arcos con peso
data Arco v p = Arc v p
  deriving (Show, Eq)

--- Tipo para listas de arcos
type Arcos v p = [Arco v p]

--- Clase para grafos con pesos
class Eq v => GrafoPesos v where
  vertices4 :: [v]
  suc4 :: Num p => v -> Arcos v p
  caminos4 :: (Num p, Ord p) => v -> v -> p -> [[v]]
  caminosDesde4 :: Num p => v -> (v->p->Bool) -> (p->Bool) -> [v] -> p -> [[v]]
  tv4 :: Num p => (v,p) -> [v] -> Bool
  
  -- Métodos por defecto
  caminos4 u v pt = caminosDesde4 u (\x _ -> x==v) (> pt) [] 0
  tv4 (x,_) ys = notElem x ys
  caminosDesde4 o te tr vis p
    | te o p = [o:vis]
    | otherwise = concat [caminosDesde4 o' te tr (o:vis) np |
                          Arc o' p' <- suc4 o,
                          tv4 (o',p') vis,
                          let np = p+p',
                          not(tr np)]

--- Instancia del grafo con pesos para enteros
instance GrafoPesos Int where
  vertices4 = [1..4]
  suc4 1 = [Arc 2 30, Arc 3 20, Arc 4 40]
  suc4 2 = [Arc 4 20]
  suc4 3 = [Arc 4 50]
  suc4 _ = []

--- Ejemplos de uso para grafos con pesos
ejemplosCaminos :: [[Int]]
ejemplosCaminos = caminos4 1 4 50
