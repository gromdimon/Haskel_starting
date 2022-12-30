module Neun where


{- Input: natu ̈rliche Zahl n ∈ N0
Output: Anzahl der Nullen der Bina ̈rdarstellung von n -}

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

decimalBinar :: Int -> String
decimalBinar 0 = ""
decimalBinar num = decimalBinar (num `div` 2) ++ intToChar (num `mod` 2)

anzahlNullen :: String -> Int
anzahlNullen [] = 0
anzahlNullen (x:xs)
    | x == '0' = 1 + anzahlNullen xs
    | otherwise = anzahlNullen xs


{- (b) Input: natu ̈rliche Zahl n ∈ N0
Output: f(n) wobei f die Funktion aus Aufgabe 1.a), U ̈bung 1 ist. -}



{- (c) Input: Nicht-leere Bina ̈rsequenz (d.h. eine Zeichenkette bestehend aus Nullen und Einsen)
Output: Dezimalzahl (Int), welche durch die Bina ̈rsequenz dargestellt ist -}

bin2dez :: String -> Int
bin2dez [] = 0
bin2dez (x:xs) = bin2dez xs + help (length xs) x

help :: Int -> Char -> Int
help _ '0' = 0
help num _ = 2 ^ num
