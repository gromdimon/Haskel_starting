{- Aufgabe 1:
Implemtiere zunächst die Funktion zaehle :: Char -> String -> Int, die einen
Buchstaben c und einen String xs erhält und die Anzahl der Vorkommen des Elements c im String
ausgibt.
Implementiere nun die Funktion countEach :: String -> String -> [(Char,Int)],
die 2 Strings erhält. Im 1. String kommt jedes Zeichen genau ein Mal vor! Die Ausgabe ist eine Liste
mit Tupeln (c,n), wobei c aus dem 1. String ist und n die Häufigkeit von c im 2. String darstellt.
Beispiel: countEach “abc“ “abacca“ liefert [(‘a‘,3),(‘b‘,1),(‘c‘,2)]. Hinweis: die zaehle-Funktion be-
nutzen. -}

-- Voraussetzung: Buchstabe c und String xs
-- Ergebnis: Anzahl der Vorkommen von c in xs
zaehle :: Char -> String -> Int
zaehle c [] = 0
zaehle c (x:xs) = if c == x then 1 + zaehle c xs else zaehle c xs

-- Voraussetzung: 2 Strings
-- Ergebnis: Liste mit Tupeln (c,n), wobei c Vorkommen in N sind
countEach :: String -> String -> [(Char,Int)]
countEach [] ys = []
countEach (x:xs) ys = (x, zaehle x ys) : countEach xs ys

{- Tests: 
-}


{- Aufgabe 2:
average::[[Float]]->[Float] soll die Liste der Durchschnittswerte der Eingabelisten
erzeugen. Implementiere average mit map und einer geeigneten Hilfsfunktion.
Beispiel: average [[1,2,3],[4,5,6],[1,1,1]] → [2.0,3.0,1.0] -}

-- Voraussetzung: Liste von Listen von Floats
-- Ergebnis: Liste von Durchschnittswerten der Eingabelisten
average :: [[Float]] -> [Float]
average xs = map average' xs

