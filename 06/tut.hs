-- Erste
module Tutsechs where
import Data.Char

intToChar :: Int -> String
intToChar x
    | x == 0 = ['0']
    | x == 1 = ['1']
    | x == 2 = ['2']
    | x == 3 = ['3']
    | x == 4 = ['4']
    | x == 5 = ['5']
    | x == 6 = ['6']
    | x == 7 = ['7']
    | x == 8 = ['8']
    | x == 9 = ['9']
    | x > 9 = (intToChar (x `div` 10)  ++ intToChar (mod x 10 ) )

-- Voraussetzung: m und n sind natuerliche Zahlen
-- Ergebnis: Ziffer an der (n-m)-Stelle von n
positionNum :: Int -> Int -> Char
positionNum k num
    | length rev < k = '0'   -- Fall, wenn wir in Binar nur 0 am Anfang haben
    | otherwise = rev!!(k - 1)   -- Finden 2k-Stelle Char in der reversiven Bina ̈rdarstellung von n
    where rev = reverse (intToChar num)

numm = read "345"::Int   -- String to Int

{- Input: Gewicht in Kg und Größe in cm einer Person als Gleitkommazahlen
Output: BMI der Person -}
bmicalc :: Float -> Float -> Float
bmicalc kg cm = (kg * cm) / 100

