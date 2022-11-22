-- Informatik A WiSe 20/21
-- 4.12.2020
-- Max Willert

module Ausdruecke where
import Data.Char

-- Zentrale Konzepte: Ausdrücke und Funktionen.
-- Ausdrücke haben immer einen Wert und einen Typ.
-- Ein Haskellprogramm berechnet den Wert eines gegebenen Ausdrucks.
-- Mit :t kann man den Typ eines gegebenen Ausdrucks anzeigen lassen.
-- Es gibt unterschiedliche Arten von Ausdrücken:

-- 1. Konstanten: 13, 16.78, 'a', True, "Informatik A", [1,3,2], ...

-- 2. Arithmetische Ausdrücke
a :: Int
a = 3*(4+6)-3

-- 3. Boolesche Ausdrücke
b :: Bool
b = True || (3>=4) && False

-- 4. Bedingte Ausdrücke
c :: Int
c = if 3>=0 then (4*5) else (3-7)
d :: String
d = case (4-3) of 1 -> "Eins" ; 2 -> "zwei" ; otherwise -> "sonst"

-- 5. Funktionsaufrufe.
e :: String
e = reverse "Hello" ++ reverse "Welt"
f :: Int
f = 13`div`5
g :: Char
g = chr 108

-- Der Typ eines Ausdrucks gibt an, welche Werte möglich sind und
-- welche Operationen auf den Werten erlaubt sind.
-- 3+4 und True || False sind erlaubt, 3+True und 4||12 hingegen nicht. 
