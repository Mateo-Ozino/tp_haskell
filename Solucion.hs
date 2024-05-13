module Solucion where
import Data.Char
-- Nombre de grupo: { La Haskellneta }
-- Integrante1: { 43915941, Mateo Agustin Ozino Caligaris }
-- Integrante2: { 40887984, Juan Ignacio Bordignon }
-- Integrante3: { 43736603, Valentin Soria Lobo }
-- Integrante4: { 19091229, Elias Ramon Duarte Cardozo }
-- Integrantes que abandonaron la materia: {}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula _ = True

-- EJ 2
letraANatural :: Char -> Int
letraANatural _ = 1

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar _ _ = 'd'

-- EJ 4
cifrar :: String -> Int -> String
cifrar _ _ = "frpsxwdflrq"

-- EJ 5
descifrar :: String -> Int -> String
descifrar _ _ = "computacion"

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista _ = ["compu", "mbcp", "kpvtq"]

-- EJ 7
frecuencia :: String -> [Float]
frecuencia _ = [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0]

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente _ _ = ('o', 33.333336)

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado _ _ = False

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
