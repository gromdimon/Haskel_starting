-- ALGORITHMEN AUF ZAHLEN
module Azwei where
import Data.Char



-- B
{- Input: natu ̈rliche Zahl n ∈ N0
Output: Anzahl der Nullen in der Bina ̈rdarstellung von n (Beispiel: 18 􏰀→ 3, denn 1810 = 100102) -}

-- Voraussetzung: a ist natuerliche Zahl
-- Ergebnis: Anzahl der Nullen in der Bina ̈rdarstellung des a
-- Found at https://stackoverflow.com/questions/20659810/haskell-int-to-char
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

-- Voraussetzung: a ist natuerliche Zahl
-- Ergebnis: String von binaere a
-- Funktion macht Binaerdarstellung von dezimale Zahl durch teilen durch 2
decimalBinar :: Int -> String
decimalBinar 0 = ""
decimalBinar num = decimalBinar (num `div` 2) ++ intToChar (num `mod` 2)

-- Voraussetzung: a ist Binarzahl
-- Ergebnis: Anzahl von 0 im Zahl
-- Funktion prueft rekursiv jedes Zahl (ob es 0 ist) und summiert, falls 0 ist
anzahlNullen :: String -> Int
anzahlNullen [] = 0
anzahlNullen (x:xs) = if digitToInt x == 0 then 1 + anzahlNullen xs else anzahlNullen xs

-- Voraussetzung: a ist Natuerliche Zahl
-- Ergebnis: Anzahl von 0 im Zahl in Binaerdarstellung
anzahl :: Int -> Int
anzahl x
    | x == 0 = 1   -- Wenn 0 , dann es ist schon 0
    | x > 0 = anzahlNullen (decimalBinar x)
    | otherwise = error"Error!!!!"

{- Tests:
> anzahl 0
1
> anzahl 2
1
> anzahl 8
3
> anzahl 18
3 -}



-- C
{- Input: zwei natu ̈rliche Zahlen n, k ∈ N0
Output: Ziffer an der 2k-Stelle der Bina ̈rdarstellung von n (Beispiel: 22, 3 􏰀→ 0, denn 2210 = 101102) -}

-- Voraussetzung: n und k sind natuerliche Zahlen
-- Ergebnis: Ziffer an der 2k-Stelle der Bina ̈rdarstellung von n
positionNum :: Int -> Int -> Char
positionNum num k
    | length binar < pos = '0'   -- Fall, wenn wir in Binar nur 0 am Anfang haben
    | otherwise = revbin!!(pos - 1)   -- Finden 2k-Stelle Char in der reversiven Bina ̈rdarstellung von n
    where binar = decimalBinar num; pos = 2^k; revbin = reverse binar

{- Tests:
> positionNum 22 3
'0'
> positionNum 22 1
'1'
> positionNum 3 1
'1'
> positionNum 2 4
'0' -}




-- D
{- Input: zwei natu ̈rliche Zahlen n, m ∈ N0
Output: Anzahl verschiedener Stellen der Dezimaldarstellungen von n und m
(Beispiel: 123456, 3466 􏰀→ 3, denn die Stellen 101, 104 und 105 unterscheiden sich.) -}

-- Voraussetzung: n und m sind natuerliche Zahlen
-- Ergebnis: Anzahl verschiedener Stellen der Dezimaldarstellungen von n und m
match :: Int -> Int -> Int
match 0 0 = 0   -- Wenn es keine 10^x mehr gibt
match 0 y = 1 + match 0 (y `div` 10)   -- Anzahlen Stellen an y, die x einfach nicht hat
match x 0 = 1 + match (x `div` 10) 0   -- Anzahlen Stellen an x, die y einfach nicht hat
match x y = if x `mod` 10 == y `mod` 10 then 1 + match (x `div` 10) (y `div` 10) else match (x `div` 10) (y `div` 10)   -- Vergleichen Ziffern und weiterfuehren mit Rekursion

{- Tests:
> match 2 2
1
> match 34 34
2
> match 2 345
2
> match 2 245
2
> match 5 345
3 -}