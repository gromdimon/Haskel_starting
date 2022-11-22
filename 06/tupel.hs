-- Informatik A WiSe 20/21
-- 4.12.2020
-- Max Willert

module Tupel where

-- Zusammengesetzte Datentypen: Tupel
-- Feste Anzahl von Werten mit ggf. unterschiedlichem Typ

type Datum = (Int,Int,Int)
-- bei Verwendung von type spricht man von einem Typsynonym

sonntag :: Datum
sonntag = (6,12,2020)

-- Achtung: Diese Funktion ist noch nicht korrekt! Monats- und
-- Jahresübergänge müssen beachtet werden.
morgen :: Datum -> Datum
morgen (t, m, j) = (t+1, m, j)

-- Vor.: Das Eingabetupel ist ein gültiges Datum.
-- Erg.: Liefert genau dann True, wenn der Eingabetag der 27.11.2020 ist.
einTollerTag :: Datum -> Bool
einTollerTag (t, m, j) = t == 27 && m == 11 && j == 2020

type Person = (String, Int) -- (Name, Alter)

maexchen :: Person
maexchen = ("Max Willert", 22)

gruessen :: Person -> String
gruessen (name, alter) = "Hallo, mein Name ist " ++ name ++ " und\
                          \ ich bin " ++ (show alter) ++ " Jahre alt."

-- Bemerkung: Die Funktion show wandelt ein Objekt in ein String um.
