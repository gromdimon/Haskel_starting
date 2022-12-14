-- Informatik A WiSe 20/21
-- 04.01.2021
-- Max Willert
module Sortieren where

-- Laufzeit von Sortieralgorithmen:
-- 1. Man kann die Zeit messen, die ein Sortierverfahren benötigt. Diese
-- ist dann aber abhängig von der Hardware.
-- 2. Man kann die Anzahl der Vergleiche zählen, die man zwischen Elementen
-- einer Liste machen muss. Diese ist dann nicht mehr abhängig von der
-- Hardware.

-- Insertsort ==========================================================
-- Sortierverfahren nach dem Prinzip "Stupid Choice & Smart Insertion"
-- Generelles Vorgehen: Lege den Anfang der Liste kurz beiseite und sortiere
-- den Rest der Liste rekursiv. Füge nun den Anfang der ursprünglichen Liste
-- in die sortierte Restliste an der richtigen Stelle ein.
-- Insertsort benötigt im Best-Case ungefähr n und im Average- und Worst-
-- Case ungefähr n^2 viele Vergleiche.
-- Es gibt noch ein Sortierverfahren, dass nach dem umgekehrten Prinzip
-- funktioniert: "Smart Choice & Stupid Insertion".

-- Vor.: keine
-- Erg.: Eine Liste, welche die Eingabeliste aufsteigend sortiert hat,
--       ist geliefert.
insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

-- Vor.: Die Eingabeliste ist aufsteigend sortiert.
-- Erg.: Eine Liste, welche das einzelne Element y an der richtigen
--       Stelle der Eingabeliste enthält, ist geliefert.
insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y (x:xs)
     | x < y = x : insert y xs -- 1. Fall: Stelle noch nicht gefunden
     | otherwise = y : x : xs -- 2. Fall: Stelle gefunden

-- Mergesort ===========================================================
-- Sortierverfahren nach dem Prinzip Teile-und-Herrsche,
-- genauer "Stupides Teilen & Intelligentes Herrschen"
-- Generelles Vorgehen: Teile die Liste in der Mitte, sortiere die
-- beiden Teillisten rekursiv und merge (verschmelze) die beiden
-- sortierten Teillisten zu einer sortierten Gesamtliste
-- Mergesort benötigt ungefähr n*log(n) viele Vergleiche (im Best-, Worst-
-- und Average-Case).
-- Es gibt noch ein Sortierverfahren, dass nach dem umgekehrten Prinzip
-- funktioniert: "Intelligentes Teilen & Stupides Herrschen".

-- Vor.: keine
-- Erg.: Eine Liste, welche die Eingabeliste aufsteigend sortiert hat,
--       ist geliefert.
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort links) (mergeSort rechts)  
         where (links,rechts) = halbiere xs

-- Vor.: keine
-- Erg.: Zwei Listen ls und rs sind geliefert, für die gilt ls++rs==xs
--       und die Länge beider Listen unterscheidet sich um höchstens 1
halbiere :: Ord a => [a] -> ([a], [a])
halbiere xs = help [] xs where
    help ys xs
     | length xs - length ys <= 1 = (ys,xs)
     | otherwise = help (ys ++ [head xs]) (tail xs)

-- Vor.: Die beiden Eingabelisten sind aufsteigend sortiert.
-- Erg.: Eine aufsteigend sortierte Liste ist geliefert, welche genau die
--       Elemente der beiden Eingabelisten enthält.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
     | x < y = x : merge xs (y:ys)
     | otherwise = y : merge (x:xs) ys
