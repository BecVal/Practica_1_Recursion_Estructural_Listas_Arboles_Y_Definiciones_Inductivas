import Text.XHtml (rev)
{-
    Práctica 1 - Recursión Estructural, Listas, Árboles y
                 Definiciones Inductivas
    Profesor: Dr. Marco Vladimir Lemus Yañez
    Ayudante: Dr. Fernando Cruz Pineda
    Fecha: 27/02/2026 

    Inetgrantes del equipo:
    - Becerra Valencia César
    - Cortes Nava José Luis
    - Díaz Anavia Javier Omar
-}

-- 4.1 Listas Definidas por el Usuario

data List a = Nil | Cons a (List a)

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

-- 4.3 Árboles Binarios

-- 4.4 Estructuras Inductivas Lógicas


