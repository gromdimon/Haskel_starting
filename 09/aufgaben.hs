module Neun where


-- Aufgabe 1
{- (a) Input: natu ̈rliche Zahl n ∈ N0
Output: Anzahl der Nullen der Bina ̈rdarstellung von n -}

anzahl :: Int -> Int
anzahl x
    | x < 0 = error"Plese enter positive value"
    | otherwise = anzahlNullen (decimalBinar x)

anzahlNullen :: String -> Int
anzahlNullen [] = 0
anzahlNullen (x:xs)
    | x == '0' = 1 + anzahlNullen xs
    | otherwise = anzahlNullen xs

decimalBinar :: Int -> String
decimalBinar 0 = ""
decimalBinar num = decimalBinar (num `div` 2) ++ intToChar (num `mod` 2)

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

{- Tests:
> anzahl 8
3
> anzahl 80
5
> anzahl 88
4
> anzahl 0
0
> anzahl 245932670
13
-}



{- (c) Input: Nicht-leere Bina ̈rsequenz (d.h. eine Zeichenkette bestehend aus Nullen und Einsen)
Output: Dezimalzahl (Int), welche durch die Bina ̈rsequenz dargestellt ist -}

charToint :: Char -> Int
charToint x
    | x == '0' = 0
    | x == '1' = 1
    | x == '2' = 2
    | x == '3' = 3
    | x == '4' = 4
    | x == '5' = 5
    | x == '6' = 6
    | x == '7' = 7
    | x == '8' = 8
    | x == '9' = 9
    | otherwise = error"IDK"

binTodez :: String -> Int
binTodez [] = error"Give a proper binary number"
binTodez (x:xs)
    | xs == [] && x == '0' = 0
    | xs == [] && x == '1' = 1
    | otherwise = ((charToint x) * 2 ^ length xs) + binTodez xs

{- Tests:
> binTodez "000"
0
> binTodez "1000"
8
> binTodez "1110"
14
-}



-- Aufgabe 2
{- Mithilfe der ZF-Notation ist es mo ̈glich, in Haskell Listen generieren zu lassen. Die ZF-Notation (benannt nach Zermelo und
Fraenkel) stammt aus der axiomatischen Mengen- lehre. Zum Beispiel ist {3 · n | n ∈ {1, . . . , 100}, n gerade} die Menge aller
Elemente 3 · n, wobei n aus der Menge {1, . . . , 100} kommt und gerade ist. In Haskell kann man schreiben:
[ 3*x | x <- [1..100], even x]. Hierbei handelt es sich um die Liste aller Elemente 3*x wobei x aus der urspru ̈nglichen Liste
[1..100] kommt und gerade ist.
Allgemein sehen Listengeneratoren folgendermaßen aus: Sei xs::[a] eine Liste, f::a->b eine Funktion und p::a->Bool ein Pra ̈dikat.
[ f x | x <- xs, p x] ist die Liste aller f x, wobei x aus der Liste xs entstammt und die Bedingung p erfu ̈llt.
Hinweis: Man beachte aber, dass es sich bei dem mathematischen Begriff der Menge um eine ungeordnete Struktur handelt, wa ̈hrend
die Elemente in Listen natu ̈rlich eine Reihenfolge haben.
(a) Gib 3 spannende Haskellausdru ̈cke an, in denen ZF-Notation vorkommt. Gib au- ßerdem jeweils den Wert und Typ dieser Ausdru ̈cke
an. Mindestens einer der Aus- dru ̈cke muss den Typ Char besitzen. -}





{-
(d) Implementiere eine Funktion zfElem, die pru ̈ft, ob ein Element in einer Liste ent- halten ist.
Die Funktion soll ZF-Notation sowie foldl verwenden. -}