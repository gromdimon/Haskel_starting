module Neun where
import Data.Char


-- Aufgabe 1
{- (a) Input: natu ̈rliche Zahl n ∈ N0
Output: Anzahl der Nullen der Bina ̈rdarstellung von n -}

-- Shorted version of next functions
-- Make steps of division by 2 and akkumulating current value of number of 0
anznull :: Int -> Int -> Int
anznull anz 0 = anz
anznull anz 1 = anz
anznull anz num = anznull (anz + val) (num `div` 2)
    where val = case (num `mod` 2) of 0 -> 1
                                      1 -> 0
{- Tests:
> anznull 0 8
3
> anznull 0 80
5
> anznull 0 88
4
> anznull 0 0
0
> anznull 0 245932670
13
-}

-- Funktion liefert anzahl der Nullen in Binardarstellung davon
anzahl :: Int -> Int
anzahl x
    | x < 0 = error"Plese enter positive value"
    | otherwise = anzahlNullen (decimalBinar x)

-- Funktion liefert anzahl der Char `0` in String
anzahlNullen :: String -> Int
anzahlNullen [] = 0
anzahlNullen (x:xs)
    | x == '0' = 1 + anzahlNullen xs
    | otherwise = anzahlNullen xs

-- Funktion wandelt decimal Zahl in Binaer String um
decimalBinar :: Int -> String
decimalBinar 0 = ""
decimalBinar num = decimalBinar (num `div` 2) ++ intToChar (num `mod` 2)

-- Funktion mapt Ziffer zu Char
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

-- Funktion wandelt Binaer String in decimal Int
binTodez :: String -> Int
binTodez [] = error"Give a proper binary number"
binTodez (x:xs)
    | xs == [] && x == '0' = 0
    | xs == [] && x == '1' = 1
    | otherwise = ((charToint x) * 2 ^ length xs) + binTodez xs

-- Funktion mapt Ziffer Char zu Int
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

-- Alle Ausdruecke sind Listen, in zweitem Ausdrueck erscheint Char
erstes = [a ^ a | a <- [0 .. 10], a`mod`2 == 0]
zweites = [ord x | x <- "string", x /= 'g']
drittes = [2 * g | g <- [ord x | x <- "string", x == 'g'], g >= 3]
{-
> erstes
[1,4,256,46656,16777216,10000000000]
> zweites
[115,116,114,105,110]
> drittes
[206]
-}



{- (d) Implementiere eine Funktion zfElem, die pru ̈ft, ob ein Element in einer Liste ent- halten ist.
Die Funktion soll ZF-Notation sowie foldl verwenden. -}

-- Funktion entspricht der Aufgabe
-- `\` bedeutet Lambda funktion, aehnlich zu `lambda acc x = acc || x == elem`
zfElem :: Eq a => a -> [a] -> Bool
zfElem elem lst = foldl (\acc x -> acc || x == elem) False lst

{- Tests:
> zfElem 1 [1,2,3,4]
True
> zfElem 'a' "abdfaradzf"
True
> zfElem True [True, False, False]
True
-}