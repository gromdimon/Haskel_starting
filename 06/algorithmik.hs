-- Informatik A WiSe 20/21
-- 4.12.2020
-- Max Willert

module Algorithmik where

-------------------------------------------------------------
-- Algorithmisches Problem 1:
-- Input: Ziffer z aus {0,...,9}, natürliche Zahl n
-- Frage: Beinhaltet die Dezimaldarstellung von n die Ziffer z?

-- Lösungsidee: siehe handgeschriebenes Skript

-- Lösung:
-- Vor.: 0 <= z <= 9 und n >= 0
-- Erg.: True ist genau dann geliefert, wenn die Dezimaldarstellung
--       von n die Ziffer z enthält.
enthaeltZiffer :: Int -> Int -> Bool
enthaeltZiffer z n
    | z < 0 || z > 9 || n < 0 = error"Voraussetzung nicht erfüllt"
    | 0 <= n && n <= 9 = z == n
    | otherwise = z == n`mod`10 || enthaeltZiffer z (n`div`10)

-------------------------------------------------------------
-- Algorithmisches Problem 2:
-- Input: natürliche Zahl n
-- Output: Quersumme der Dezimaldarstellung von n

-- Lösungsidee: siehe handgeschriebenes Skript

-- Lösung:
-- Vor.: n >= 0
-- Erg.: Die Quersumme von n ist geliefert.
quersumme :: Int -> Int
quersumme n
    | n < 0 = error"Voraussetzung nicht erfüllt"
    | n > 10 = n`mod`10 + quersumme (n`div`10)
    | otherwise = n

-------------------------------------------------------------
-- Algorithmisches Problem 3:
-- Input: Natürliche Zahl n
-- Output: Ziffer der Hunderterstelle der Dezimaldarstellung
--         von n. Ist n < 100, so ist die Hunderterstelle gleich 0.

-- Lösungsidee: Wir teilen die Zahl n ganzzahlig (d.h. mit Hilfe von
-- div) durch 100. Die Einerstelle der so erzeugten Zahl n' entspricht
-- nun der gesuchten Hunderterstelle von n. Beim ganzzahligen Teilen
-- von n' durch 10 entsteht ein Rest. Dieser Rest ist unsere gesuchte
-- Ziffer.

-- Lösung:
-- Vor.: n >= 0
-- Erg.: Die Ziffer der Hunderterstelle von n ist geliefert.
hunderter :: Int -> Int
hunderter n
     | n >= 0 = (n`div`100)`mod`10
     | otherwise = error"Die Zahl darf nicht negativ sein."
