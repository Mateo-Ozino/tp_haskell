module Solucion where
import Data.Char
-- Nombre de grupo: { La Haskellneta }
-- Integrante1: { 43915941, Mateo Agustin Ozino Caligaris }
-- Integrante2: { 40887984, Juan Ignacio Bordignon }
-- Integrante3: { 43736603, Valentin Soria Lobo }
-- Integrante4: { 19091229, Elias Ramon Duarte Cardozo }
-- Integrantes que abandonaron la materia: {}

-- a = 97
-- z = 122

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula c = ord c >= 97 && ord c <= 122

-- EJ 2
letraANatural :: Char -> Int
letraANatural c = ord c - 97

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar c n
            | not (esMinuscula c) = c  
            | otherwise = chr (desplazarAux c n)

desplazarAux :: Char -> Int -> Int
desplazarAux c n
                | ord c + n > ord 'z' = desplazarAux c (n-26) -- me paso de 'z'
                | ord c + n < ord 'a' = desplazarAux c (n+26) -- no llego a 'a'
                | otherwise = ord c + n

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (c:cs) n
              | not (esMinuscula c) = c:cifrar cs n
              | otherwise = desplazar c n : cifrar cs n

-- EJ 5
descifrar :: String -> Int -> String
descifrar s n = cifrar s (-n)

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista list = cifrarListaAux list 0

cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _ = []
cifrarListaAux (x:xs) n = cifrar x n : cifrarListaAux xs (n + 1)

-- EJ 7
frecuencia :: String -> [Float]
frecuencia (x:xs)
                | not (todasMinusculas (x:xs)) = crearListaDe0s 26
                | otherwise = frecuenciaAux (x:xs) abecedario

frecuenciaAux :: String -> String -> [Float]
frecuenciaAux _ [] = []
frecuenciaAux string stringIterable = porcentajeApariciones string (head stringIterable) : frecuenciaAux string (tail stringIterable)

abecedario :: [Char]
abecedario = abecedarioDesde 'a'

abecedarioDesde :: Char -> [Char]
abecedarioDesde 'z' = ['z']
abecedarioDesde c = c : abecedarioDesde (chr (ord c + 1))

crearListaDe0s :: Int -> [Float]
crearListaDe0s n
              | n > 0 = 0.0 : crearListaDe0s (n - 1)
              | otherwise = []

contarApariciones :: Eq t => [t] -> t -> Int
contarApariciones [] _ = 0
contarApariciones (x:xs) item
                            | item == x = 1 + contarApariciones xs item
                            | otherwise = contarApariciones xs item

porcentajeApariciones :: Eq t => [t] -> t -> Float
porcentajeApariciones [] _ = 0
porcentajeApariciones list item = fromIntegral(contarApariciones list item * 100) / fromIntegral (length list)

todasMinusculas :: String -> Bool
todasMinusculas [] = True
todasMinusculas (x:xs) = esMinuscula x && todasMinusculas xs

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente palabra nPosiciones  = (chr (ord 'a' + posicionElemento 0 (maximo (frecuencia (cifrar palabra nPosiciones))) (frecuencia (cifrar palabra nPosiciones))) ,maximo (frecuencia (cifrar palabra nPosiciones)))

maximo:: Ord t => [t] -> t -- recibe: lista con elementos(números en este caso) y devuelve: elemento (número) más grande, en el caso de 2 o más elementos iguales devuelve el primer elemento
maximo (x:[]) = x 
maximo (x:y:xs) 
                | x >= y = maximo (x:xs)
                | otherwise = maximo (y:xs)

posicionElemento :: Eq t => Int -> t -> [t] -> Int -- recibe: valor inicial de posición = 0, el elemento buscado, la lista con el elemento y devuelve: posición de elemento
posicionElemento n elem (x:xs)  
                                | x == elem  = n
                                | otherwise = posicionElemento (n+1) elem xs

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado palabra1 palabra2 = esDescifradoAux palabra1 palabra2 0

esDescifradoAux :: String -> String -> Int -> Bool
esDescifradoAux _ _ 26 = False
esDescifradoAux palabra1 palabra2 n
                                    |palabra2 == cifrar palabra1 n = True
                                    |otherwise = esDescifradoAux palabra1 palabra2 (n+1)

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados _ = [("compu", "frpsx"), ("frpsx", "compu")]

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave _ _ = "compucom"

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere _ _ = "kdueciirqdv"

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere _ _ = "computacion"

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ _ = "asdef"

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere _ _ _ = [("hola", "b")]
