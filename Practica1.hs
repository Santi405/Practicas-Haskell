{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.List

-- Capo Wassers


{-

EJERCICIO 1

Los siguientes códigos tienen errores, cargar el archivo 20.Practica.0.hs en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)
regla b = case b of
    True -> "Quedate en Casa"
    False -> "Qudate en Casa"

-- b) El error era que la funcion se llamaba case (palabra reservada)
fun []          =  []
fun [x]         =  []
fun (x:y:xs)    =  y : fun (x:xs)

-- c) Mismo error que en el b
mmap f []        =  []
mmap f (x:xs)     =  (f x) : mmap f xs

-- d) No se pueden hacer lsitas de elementos de distinto tipo
listNumeros = (1 , 2) : (2 , 1) : []

-- e) no se
--function []     ++! ys = ys
--function (x:xs) ++! ys = x : xs ++! ys

-- f) PARENTESIS Y NO SIGNO DE MAS
addToTail x xs = map x (tail xs)

-- g) parentesis al usar funciomn??
listmin xs = head (sort xs)

-- h) (*) Muerte y destruccion
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = (f x) : smap f xs

{-

EJERCICIO 2

Definir las siguientes funciones y determinar su tipo:

a) five, que dado cualquier valor, devuelve 5
-}

five x = 5


{-
b) apply, que toma una función y un valor, y devuelve el resultado de
aplicar la función al valor dado
-}

apply f x = f x


{-
c) identidad, la función identidad
-}

identidad x = x


{-
d) first, que toma un par ordenado, y devuelve su primera componente
-}

first (x , y) = x


{-
e) derive, que aproxima la derivada de una función dada en un punto dado eh?
-}

derive f x h = (f (x+h) - f x)/h


{-
f) sign, la función signo
-}

sign x = if (x >= 0) then
            1
        else
            (-1)

{-
g) vabs, la función valor absoluto (usando sign y sin usarla)
-}

vabsSign x = x * (sign x)
vabs x = if sign x == 1 then
           x
        else
            (-1) * x

{-
h) pot, que toma un entero y un número, y devuelve el resultado de
elevar el segundo a la potencia dada por el primero
-}

pot 0 x = 1
pot z x = x * x * pot (z-1) x


{-
i) xor, el operador de disyunción exclusiva
-}

xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False


{-
j) max3, que toma tres números enteros y devuelve el máximo entre llos
-}

max3 x y z | x > y && x > z = x
           | y > x && y > z = y
           | otherwise = z


{-
k) swap, que toma un par y devuelve el par con sus componentes invertidas
-}

swap (x, y) = (y, x)

{-

EJERCICIO 3

Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

es_bisiesto :: Integral a => a -> Bool
es_bisiesto x = (mod x 400 == 0) || ((mod x 4 == 0) && mod x 100 /= 0)

{-

EJERCICIO 4

Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
a) (Int -> Int) -> Int
-}
fun1:: (Int -> Int) -> Int
fun1 f = f 2

{-
b) Int -> (Int -> Int)
-}
fun2:: Int -> (Int->Int)
fun2 x y = x + y

{-
c) (Int -> Int) -> (Int -> Int)
-}
fun3:: (Int -> Int) -> (Int -> Int)
fun3 f x = f x

{-
d) Int -> Bool
-}
fun4:: Int -> Bool
fun4 x = x==4

{-
e) Bool -> (Bool -> Bool)
-}

fun5:: Bool -> Bool -> Bool
fun5 x y = x && y

{-
f) (Int,Char) -> Bool
-}
fun6:: (Int, Char) -> Bool
fun6 (x, c) = x == 4 -- XD

{-
g) (Int,Int) -> Int
-}
fun7:: (Int, Int) -> Int
fun7 (x,y) = x + y

{-
h) Int -> (Int,Int)
-}
fun8:: Int-> (Int, Int) 
fun8 x = (x,x)

{-
i) a -> Bool
-}

fun9 :: a -> Bool
fun9 a = True

{-
j) a -> a
-}

fun10 :: p -> p
fun10 a = a

{-

EJERCICIO 5

Definir las siguientes funciones usando listas por comprensión:

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
-}


divisors n = [x | x <- [1..n], n `mod` x == 0]

{-
b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'
-}

matches z zs = [x | x <- zs, x == z]


{-
c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'
-}

cuadrupla n = [ (a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a^2 + b^2 == c^2 + d^2]


{-
(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
unique :: [Int] -> [Int]
-}

primerAparicion z [] = 0
primerAparicion z ((x,y):xs) = if x == z then y else primerAparicion z xs

unique xs = [x | (x,y) <- zip xs [1..], y == primerAparicion x (zip xs [1..])]

{-

EJERCICIO 6 

El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

scalarProduct xs ys = [x + y | (x,d) <- zip xs [0..], (y,z) <- zip ys [0..], z == d]


{-

EJERCICIO 7 

Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números
-}

suma [] = 0
suma (x:xs) = x + (suma xs)
{-
b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario
-}
alguno [] = False
alguno (x:xs) = if x then True else alguno xs

{-
c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario
-}

todos [] = False
todos [x] = x
todos (x:xs) = if x then alguno xs else False

{-
d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales
-}

-- Ni idea que es un ordinal xd

{-
e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado
-}

restos y [] = []
restos y (x:xs) = (x `mod` y):restos y xs 

{-
f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados
-}

cuadrados [] = []
cuadrados (x:xs) = (x^2):cuadrados xs

{-
g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes
-}

longitudes [] = []
longitudes (x:xs) = (length x):longitudes xs

{-
h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda
-}

orden [] = []
orden ((x,y):xs) = if x > 3*y then
                                  (x,y):orden xs
                              else
                                  orden xs

{-
i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares
-}

pares [] = []
pares (x:xs) | x `mod` 2 == 0 = x:pares xs
             | otherwise = pares xs

{-
j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)
-}

--much otexto

{-
k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}

masDe [] n = []
masDe (xs:xss) n | length xs > n = xs:masDe xss n
                 | otherwise = masDe xss n


{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/
-}

-- No hay chance

main = print (scalarProduct [1,2,3,4,5] [1,2,3,4,5])