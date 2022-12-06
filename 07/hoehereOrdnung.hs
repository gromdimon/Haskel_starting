:-- Informatik A WiSe 20/21
-- 14.12.2020
-- Max Willert
module HoehereOrdnung where
import Data.Char 

-- Unterschiedliche Notationen
-- Präfixnotation: Der Funktionsname steht VOR den Parametern
-- add 3 4 liefert 7
-- (+) 3 4 liefert 7
-- Infixnotation: Der Funktionsname steht ZWISCHEN den Parametern
-- Das geht nur bei zweistelligen Funktionen
-- x`add`4
-- 3 + 4

-- Wir könnten definieren add x y = x+y. Diese Definition ist so zu verstehen,
-- dass add von x und y die Summe von x und y ist. Man kann add aber auch
-- folgendermaßen definieren:
add :: Integer -> Integer -> Integer
add = (+)
-- Hier sagen wir: Die Funktion add soll bitte dasselbe wie die Funktion
-- (+) tun.

-- Vor.: keine
-- Erg.: Der Nachfolger der Eingabezahl ist geliefert.
inkrement :: Integer -> Integer
inkrement = (+1)

-- Wir verwenden bei Funktionen nicht die Schreibweise f(x,y) wie es in
-- der Mathematik üblich ist, sondern die Curry-Schreibweise: f x y
-- Damit können wir zum Beispiel folgendes tun:
-- (add 1) ist eine Funktion, die EINE Zahl x erhält und x+1 liefert,
-- (add 1) :: Integer -> Integer
-- Außerdem könnten wir definieren: inkrement = add 1

-- Funktionen höherer Ordnung ==========================================
-- Eine Funktion ist höhere Ordnung, wenn sie als Eingabeparameter min.
-- eine Funktion erhält.

-- Vor.: keine
-- Erg.: True ist genau dann geliefert, wenn f angewendet auf x >= 3 ist.
test :: (a -> Integer) -> a -> Bool -- Die Klammern in der Signatur sind wichtig!!
test f x = (f x >= 3)
{- Tests:
test inkrement 2 liefert True
test (div 2) 4 liefert False
-}

-- Vor.: keine
-- Erg.: Die 2-fache Komposition der Eingabefunktion ist geliefert.
twice :: (a -> a) -> a -> a
twice f = f.f

{- Tests:
(.) ist die Komposition von Funktionen
Es gilt: (f.g) x = f (g x)
-}

-- ... und was macht diese Funktion?
caesar :: Char -> Char
caesar = chr.(+3).ord
{-Tests:

-}

-- Wichtige Funktionen Höherer Ordnung----------------------------------

-- Vor.: keine
-- Erg.: Eine Liste, bei der auf jedes Element die Funktion f angewendet ist,
--       ist geliefert.
-- map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x : map f xs

-- Beispiele:

-- Vor.: keine
-- Erg.: Eine Liste, bei der jedes Element durch seinen Nachfolger ersetzt ist,
--       ist geliefert.
inkrementAll xs = map inkrement xs

-- Vor.: keine
-- Erg.: Eine Liste, bei der jedes Element verdoppelt ist,
--       ist geliefert.
doubleAll xs = map (*2) xs

-- Vor.: keine
-- Erg.: Eine Liste der Ordinalzahlen der Eingabeliste ist geliefert.
ordAll xs = map ord xs

-- Was tut map (map inkrement) xss ???

-- Vor.: keine
-- Erg.: Eine Liste aller Elemente, die das Prädikat p erfüllen, ist geliefert.
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p [] = []
-- filter p (x:xs)
--   | p x = x: filter p xs
--   | otherwise = filter p xs

-- Beispiele:
-- filter even [9,23,4,5,8] liefert [4,8] (nur die geraden Zahlen)
-- filter isLower "Hallo Welt" liefert "alloelt" (nur die Kleinbuchstaben)
-- filter (even.inkrement) [9,23,4,5,8] liefert [9,23,5]

-- Vor.: keine
-- Erg.: Die Rechtsfaltung mit neutralem Element e und Funktion f
--       der Eingabeliste ist geliefert.
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f e [] = e
-- foldr f e (x:xs) = x`f`(foldr f e xs)

-- Beispiele:
-- Summe aller Zahlen einer Liste: sum = foldr (+) 0
-- Produkt aller Zahlen einer Liste: produkt = foldr (*) 1
-- Veroderung aller Booleans einer Liste: verorderung = foldr (||) False

-- Vor.: keine
-- Erg.: Die Linksfaltung mit neutralem Element e und Funktion f
--       der Eingabeliste ist geliefert.
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f e [] = e
-- foldl f e (x:xs) = foldl f (f e x) xs
