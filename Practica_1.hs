import Prelude hiding (zip)
{-
    Práctica 1 - Recursión Estructural, Listas, Árboles y
                 Definiciones Inductivas
    Profesor: Dr. Marco Vladimir Lemus Yañez
    Ayudante: Dr. Fernando Cruz Pineda
    Fecha: 27/02/2026 

    Integrantes del equipo:
    - Becerra Valencia César
    - Cortes Nava José Luis
    - Díaz Anavia Javier Omar
-}

-- 4.1 Listas Definidas por el Usuario

data List a = Nil | Cons a (List a) deriving (Show)

{-  
    Función reverseL: Toma una lista y devuelve una nueva lista con los elementos en orden inverso.
    Caso base: Si la lista original ya está vacía, regresamos lo que juntamos.
    Caso recursivo: Tomamos el primer elemento y lo ponemos al inicio del acumulador.
    Luego, repetimos el proceso con el resto de la lista.
-}
reverseL :: List a -> List a
reverseL listaInicial = auxiliar listaInicial Nil
  where
    auxiliar Nil acumulador = acumulador
    auxiliar (Cons elemento resto) acumulador = auxiliar resto (Cons elemento acumulador)

{-
    Función zip: Toma dos listas del mismo tamaño y una función binaria, y devuelve una nueva lista
    donde cada elemento es el resultado de aplicar la función a los elementos correspondientes de las dos listas.
    Caso base: Si ambas listas están vacías, regresamos una lista vacía.
    Caso recursivo: Si ambas listas tienen al menos un elemento.
    Caso de seguridad: Por si acaso las listas llegan a ser de diferente tamaño, 
    detenemos el proceso devolviendo Nil.
-}
zip :: (a -> b -> c) -> List a -> List b -> List c
zip funcion Nil Nil = Nil
zip funcion (Cons elemento1 resto1) (Cons elemento2 resto2) = 
    Cons (funcion elemento1 elemento2) (zip funcion resto1 resto2)
zip _ _ _ = Nil

-- 4.2 Fold sobre Listas
{-
    Función foldrL: Reduce una lista a un solo valor, procesada de derecha a izquierda.
    Caso base: Si la lista está vacía, regresamos el valor inicial 'z'.
    Caso recursivo: Se aplica la función 'c' que combina el elemento actual 'x' con
    el resultado acumulado de procesar el resto de la lista 'XS'.
-}
foldrL :: (a -> b -> b) -> b -> List a -> b
foldrL _ z Nil = z
foldrL c z (Cons x xs) = c x (foldrL c z xs)

{-
    Función lengthL: Te da la cantidad de elementos de una lista.
    Usa foldrL con un acumulador que inicia en 0. Ignora el valor de cada elemento de la
    lista y agrega 1 al acumulador.
-}
lengthL :: List a -> Int
lengthL lista = foldrL sumarUno 0 lista
  where
    sumarUno _ acc = acc + 1

{-
    Función sumL: Suma todos los números enteros de una lista.
    Usa foldrL con un valor inicial de 0. Utiliza el operador de suma (+)
    para calcular la suma de cada elemento de la lista.
-}
sumL :: List Int -> Int
sumL lista = foldrL (+) 0 lista

{-
    Función mapL: Transforma una lista con una función 'f' aplicada a cada elemento en la lista.
    Usa foldrL iniciando con una lista vacía. Para cada elemento, aplica 'f' y usa
    'Cons' para pegar el elemento transformado a la nueva lista.
-}
mapL :: (a -> b) -> List a -> List b
mapL f lista = foldrL (Cons . f) Nil lista

-- 4.3 Árboles Binarios
data Tree a = 
      Empty 
    | Node a (Tree a) (Tree a) deriving (Show)

{-
    Función nhojas: Cuenta el número de hojas del árbol, que son los nodos sin hijos.
    Caso base 1: Si el árbol está vacío, devuelve 0.
    Caso base 2: Si el árbol es un solo nodo cuyos hijos izquierdo y derecho son vacíos,
    devuelve 1. 
    Caso recursivo: Suma la cantidad de hojas en el árbol izquierdo con las hojas del árbol
    derecho.
-}
nhojas :: Tree a -> Integer
nhojas Empty = 0
nhojas (Node _ Empty Empty) = 1
nhojas (Node _ a b) = nhojas a + nhojas b

{-
    Función inorder: Recorre el árbol en orden, con la secuencia Iz -> raíz -> Der.
    Caso base: Si llegamos a una rama vacía, regresamos una lista vacía.
    Caso recursivo: Concatenamos la lista resultante de la rama izquierda al nodo actual, y
    La lista de la rama derecha.
-}
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node x a b) = inorder a ++ [x] ++ inorder b

{-
    Función dls: Búsqueda en profundidad para saber si un elemento está en el árbol o no.
    Caso base: Si llegamos a un nodo vacío, el elemento no esta ahí. Regresamos False.
    Case recursivo: Comprobamos si el nodo actual es igual 'y' es el que buscamos,
    Si no, buscamos en la rama izquierda, si no lo encuentra, lo busca en la derecha.
-}
dfs :: Eq a => a -> Tree a -> Bool
dfs _ Empty = False
dfs x (Node y a b) =
    (x == y) || dfs x a || dfs x b

-- 4.4 Estructuras Inductivas Lógicas

data Formula = Atom String
            | Neg Formula
            | Conj Formula Formula
            | Dis Formula Formula
            | Impl Formula Formula
            | Equiv Formula Formula
            deriving (Show, Eq)

{-  
    Función profundidad: Calcula la profundidad de una fórmula lógica, que es el número máximo de niveles de anidación de operadores lógicos.
    Caso base: Si la fórmula es un átomo, su profundidad es 0.
    Caso recursivo: Para cada operador lógico (Neg, Conj, Dis, Impl, Equiv), sumamos 1 a la profundidad máxima de sus subfórmulas.
-}

profundidad :: Formula -> Int
profundidad (Atom _) = 0
profundidad (Neg f) = 1 + profundidad f
profundidad (Conj f1 f2) = 1 + max (profundidad f1) (profundidad f2)
profundidad (Dis f1 f2) = 1 + max (profundidad f1) (profundidad f2)
profundidad (Impl f1 f2) = 1 + max (profundidad f1) (profundidad f2)
profundidad (Equiv f1 f2) = 1 + max (profundidad f1) (profundidad f2)  
