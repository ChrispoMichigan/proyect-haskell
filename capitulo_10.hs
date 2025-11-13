import Data.List (union)
import Data.Maybe (fromJust)

--- 10.1 Directorios

--- Tipo de datos para representar ficheros y directorios
data Fichero = Fichero String
             | Dir String [Fichero]
  deriving (Eq, Show)

--- Tipo para representar un sistema de ficheros
type SistemaFicheros = [Fichero]

--- Ejemplo de sistema de ficheros
ejemploSistemaFicheros :: SistemaFicheros
ejemploSistemaFicheros = 
  [Fichero "apa",
   Dir "bepa" [Fichero "apa", Dir "bepa" [], Dir "cepa" [Fichero "bepa"]],
   Dir "cepa" [Dir "bepa" [], Dir "cepa" [Fichero "apa"]]]

--- 10.2 Lógica proposicional

--- Tipo para nombres de variables
type Nombre = String

--- Tipo de datos para fórmulas proposicionales
data Prop = Var Nombre
          | Prop :& Prop
          | Prop :| Prop
          | No Prop
  deriving (Eq, Show)

--- Obtiene las variables de una fórmula proposicional
vars :: Prop -> [Nombre]
vars (Var x) = [x]
vars (a :& b) = vars a `union` vars b
vars (a :| b) = vars a `union` vars b
vars (No a) = vars a

--- 10.3 Expresiones aritméticas (versión simple)

--- Tipo de datos para expresiones aritméticas simples
data Expr1 = Num Integer
           | Sum Expr1 Expr1
           | Pro Expr1 Expr1
  deriving Eq

--- Muestra una expresión aritmética simple
muestraExpr1 :: Expr1 -> String
muestraExpr1 (Num n)
  | n < 0 = "(" ++ show n ++ ")"
  | otherwise = show n
muestraExpr1 (Sum a b) = muestraExpr1 a ++ "+" ++ muestraExpr1 b
muestraExpr1 (Pro a b) = muestraFactor1 a ++ "*" ++ muestraFactor1 b

--- Función auxiliar para manejar precedencia
muestraFactor1 :: Expr1 -> String
muestraFactor1 (Sum a b) = "(" ++ muestraExpr1 (Sum a b) ++ ")"
muestraFactor1 e = muestraExpr1 e

--- Instancia Show para Expr1
instance Show Expr1 where
  show = muestraExpr1

--- 10.4 Expresiones aritméticas generales

--- Tipo de datos para expresiones aritméticas con 4 operaciones
data Expr2 = Num2 Integer
           | Sum2 Expr2 Expr2
           | Res2 Expr2 Expr2
           | Pro2 Expr2 Expr2
           | Div2 Expr2 Expr2
  deriving Eq

--- Muestra una expresión aritmética general
muestraExpr2 :: Expr2 -> String
muestraExpr2 (Num2 n)
  | n < 0 = "(" ++ show n ++ ")"
  | otherwise = show n
muestraExpr2 (Sum2 a b) = muestraExpr2 a ++ "+" ++ muestraExpr2 b
muestraExpr2 (Res2 a b) = muestraExpr2 a ++ "-" ++ muestraExpr2 b
muestraExpr2 (Pro2 a b) = muestraFactor2 a ++ "*" ++ muestraFactor2 b
muestraExpr2 (Div2 a b) = muestraFactor2 a ++ "/" ++ muestraFactor2 b

--- Función auxiliar para precedencia
muestraFactor2 :: Expr2 -> String
muestraFactor2 (Sum2 a b) = "(" ++ muestraExpr2 (Sum2 a b) ++ ")"
muestraFactor2 (Res2 a b) = "(" ++ muestraExpr2 (Res2 a b) ++ ")"
muestraFactor2 e = muestraExpr2 e

--- Instancia Show para Expr2
instance Show Expr2 where
  show = muestraExpr2

--- Calcula el valor de una expresión aritmética general
valor2 :: Expr2 -> Integer
valor2 (Num2 n) = n
valor2 (Sum2 a b) = valor2 a + valor2 b
valor2 (Res2 a b) = valor2 a - valor2 b
valor2 (Pro2 a b) = valor2 a * valor2 b
valor2 (Div2 a b) = valor2 a `div` valor2 b

--- 10.5 Expresiones aritméticas con operadores

--- Tipo para operadores aritméticos
data Ops = Suma | Resta | Producto | Division
  deriving (Eq, Show)

--- Tipo de expresión con operadores
data Expr3 = Num3 Integer
           | Op Ops Expr3 Expr3
  deriving Eq

--- Muestra expresión con operadores
muestraExpr3 :: Expr3 -> String
muestraExpr3 (Num3 n)
  | n < 0 = "(" ++ show n ++ ")"
  | otherwise = show n
muestraExpr3 (Op Suma a b) = muestraExpr3 a ++ "+" ++ muestraExpr3 b
muestraExpr3 (Op Resta a b) = muestraExpr3 a ++ "-" ++ muestraExpr3 b
muestraExpr3 (Op Producto a b) = muestraFactor3 a ++ "*" ++ muestraFactor3 b
muestraExpr3 (Op Division a b) = muestraFactor3 a ++ "/" ++ muestraFactor3 b

--- Función auxiliar para precedencia
muestraFactor3 :: Expr3 -> String
muestraFactor3 (Op Suma a b) = "(" ++ muestraExpr3 (Op Suma a b) ++ ")"
muestraFactor3 (Op Resta a b) = "(" ++ muestraExpr3 (Op Resta a b) ++ ")"
muestraFactor3 e = muestraExpr3 e

--- Instancia Show para Expr3
instance Show Expr3 where
  show = muestraExpr3

--- Calcula el valor de una expresión con operadores
valor3 :: Expr3 -> Integer
valor3 (Num3 n) = n
valor3 (Op op a b) = valorOp op (valor3 a) (valor3 b)
  where
    valorOp Suma a b = a + b
    valorOp Resta a b = a - b
    valorOp Producto a b = a * b
    valorOp Division a b = a `div` b

--- 10.6 Expresiones aritméticas con notación infija

--- Tipo con notación infija
data Expr4 = N Integer
           | Expr4 :+: Expr4
           | Expr4 :*: Expr4
  deriving Eq

--- Muestra expresión con notación infija
muestraExpr4 :: Expr4 -> String
muestraExpr4 (N n)
  | n < 0 = "(" ++ show n ++ ")"
  | otherwise = show n
muestraExpr4 (a :+: b) = muestraExpr4 a ++ "+" ++ muestraExpr4 b
muestraExpr4 (a :*: b) = muestraFactor4 a ++ "*" ++ muestraFactor4 b

--- Función auxiliar para precedencia
muestraFactor4 :: Expr4 -> String
muestraFactor4 (a :+: b) = "(" ++ muestraExpr4 (a :+: b) ++ ")"
muestraFactor4 e = muestraExpr4 e

--- Instancia Show para Expr4
instance Show Expr4 where
  show = muestraExpr4

--- Calcula el valor de expresión infija
valor4 :: Expr4 -> Integer
valor4 (N n) = n
valor4 (a :+: b) = valor4 a + valor4 b
valor4 (a :*: b) = valor4 a * valor4 b

--- 10.7 Expresiones aritméticas con variables y derivación simbólica

--- Tipo para entornos (variables y sus valores)
type Ent = [(String, Integer)]

--- Tipo de expresión con variables
data Expr5 = Num5 Integer
           | Sum5 Expr5 Expr5
           | Pro5 Expr5 Expr5
           | Var5 Nombre
  deriving Eq

--- Muestra expresión con variables
muestraExpr5 :: Expr5 -> String
muestraExpr5 (Num5 n)
  | n < 0 = "(" ++ show n ++ ")"
  | otherwise = show n
muestraExpr5 (Sum5 a b) = muestraExpr5 a ++ "+" ++ muestraExpr5 b
muestraExpr5 (Pro5 a b) = muestraFactor5 a ++ "*" ++ muestraFactor5 b
muestraExpr5 (Var5 x) = x

--- Función auxiliar para precedencia
muestraFactor5 :: Expr5 -> String
muestraFactor5 (Sum5 a b) = "(" ++ muestraExpr5 (Sum5 a b) ++ ")"
muestraFactor5 e = muestraExpr5 e

--- Instancia Show para Expr5
instance Show Expr5 where
  show = muestraExpr5

--- Calcula el valor de expresión con variables en un entorno
valor5 :: Ent -> Expr5 -> Integer
valor5 ent (Num5 n) = n
valor5 ent (Sum5 a b) = valor5 ent a + valor5 ent b
valor5 ent (Pro5 a b) = valor5 ent a * valor5 ent b
valor5 ent (Var5 x) = fromJust (lookup x ent)

--- Obtiene las variables de una expresión
vars5 :: Expr5 -> [Nombre]
vars5 (Num5 n) = []
vars5 (Sum5 a b) = vars5 a `union` vars5 b
vars5 (Pro5 a b) = vars5 a `union` vars5 b
vars5 (Var5 y) = [y]

--- Calcula la derivada de una expresión respecto a una variable
derivada :: Expr5 -> Nombre -> Expr5
derivada (Num5 n) x = Num5 0
derivada (Sum5 a b) x = Sum5 (derivada a x) (derivada b x)
derivada (Pro5 a b) x = Sum5 (Pro5 a (derivada b x)) (Pro5 b (derivada a x))
derivada (Var5 y) x
  | x == y = Num5 1
  | otherwise = Num5 0

--- 10.8 Árboles de enteros

--- Tipo de datos para árboles binarios de enteros
data Arbol = Vacio
           | Nodo Int Arbol Arbol
  deriving (Eq, Show)

--- Suma todos los nodos del árbol
sumaArbol :: Arbol -> Int
sumaArbol Vacio = 0
sumaArbol (Nodo n a1 a2) = n + sumaArbol a1 + sumaArbol a2

--- Calcula la profundidad máxima del árbol
profundidad :: Arbol -> Int
profundidad Vacio = 0
profundidad (Nodo n a1 a2) = 1 + max (profundidad a1) (profundidad a2)

--- Cuenta las ocurrencias de un valor en el árbol
ocurrencias :: Arbol -> Int -> Int
ocurrencias Vacio _ = 0
ocurrencias (Nodo n a1 a2) p
  | n == p = 1 + ocurrencias a1 p + ocurrencias a2 p
  | otherwise = ocurrencias a1 p + ocurrencias a2 p

--- Obtiene el subárbol izquierdo
izquierdo :: Arbol -> Arbol
izquierdo (Nodo _ a _) = a

--- Obtiene el subárbol derecho
derecho :: Arbol -> Arbol
derecho (Nodo _ _ a) = a
