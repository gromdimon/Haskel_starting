-- Informatik A WiSe 20/21
-- 4.12.2020
-- Max Willert

module Funktionen where

-- Funktionen in Haskell
-- ---------------------

-- Voraussetzungen: keine
-- Ergebnis: Das doppelte der Eingabezahl ist geliefert.
double :: Int -> Int
double x = 2*x

-- Voraussetzungen: y/=0
-- Ergebnis: Quotient von x und y ist geliefert.
teile :: Double -> Double -> Double
teile x y = x/y

-- Welche Möglichkeiten gibt es, um Funktionen zu definieren?
-------------------------------------------------------------
-- 1. Direkte Angabe eines Ausdrucks:

inkrement :: Int -> Int
inkrement n = n+1

intbool :: Int -> Bool -> Int
--- Int und Bool sind die Eingabytpen
--- Int entspricht dem Ausgabetyp
intbool n b = if b then n else 0

-- Theoretisch auch möglich, jedoch nicht für uns geeignet:
intboolDoof :: (Int,Bool) -> Int
intboolDoof (n,b) = if b then n else 0

-------------------------------------------------------------
-- 2. Man kann andere Funktionen benutzen, um eigene zu
-- definieren.
triple :: Int -> Int
triple x = x + double x

quadruple :: Int -> Int
quadruple x = double (double x)

-------------------------------------------------------------
-- 3. Fallunterscheidung:
absolut :: Int -> Int
absolut n
    | n<0 = -n
    | otherwise = n

sign :: Int -> Int
sign n
    | n < 0 = -1
    | n > 0 = 1
    | n == 0 = 0
    
intbool2:: Int -> Bool -> Int
intbool2 x b
    | b = x
    | otherwise = 0

-------------------------------------------------------------
-- 4. Pattern Matching (Musteranpassung)
xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

intbool3 :: Int -> Bool -> Int
intbool3 n True = n
intbool3 n False = 0

-- auch mit Platzhalter (Wildcard)
intbool4 :: Int -> Bool -> Int
intbool4 n True = n
intbool4 _ False = 0

--oder :: Bool -> Bool -> Bool
--oder True _ = True
--oder False b = b

-- man kann beliebig mischen, Definitionen
-- werden von oben nach unten abgearbeitet
sign2 :: Int -> Int
sign2 0 = 0
sign2 n 
     | n > 0 = 1
sign2 _ = -1
