-- Informatik A WiSe 20/21
-- 4.12.2020
-- Max Willert

module Rekursion where
-- Rekursion ---------------------------------------------------------
-- Bei der Definition einer Funktion kann man diese Funktion selbst 
-- wieder benutzen. Dadurch kann man einen Funktionswert auf den/die
-- Funktionswerte fuer "einfachere" Argumente zurueckfuehren.

-- Einfachrekursion
fakultaet :: Int -> Int
fakultaet 0 = 1
fakultaet n
    | n > 0 = n * fakultaet (n-1)
    | otherwise = error"Die Zahl darf nicht negativ sein."

summeVonKubiks :: Int -> Int
summeVonKubiks 0 = 0
summeVonKubiks n
    | n > 0 = n*n*n + summeVonKubiks (n-1)
    | otherwise = error"Die Zahl darf nicht negativ sein."

-- Mehrfachrekursion
-- Vor.: Eingabezahl >= 0
-- Erg.: n-te Fibonaccizahl ist geliefert.
fibonacci :: Int -> Int
fibonacci 0 = 0 -- 1. Rekursionsanker
fibonacci 1 = 1 -- 2. Rekursionsanker
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Verschränkte Rekursion
gerade :: Int -> Bool
gerade 0 = True
gerade n
    | n > 0 = ungerade (n-1)
    | otherwise = gerade (-n)

ungerade :: Int -> Bool
ungerade 0 = False
ungerade n
    | n > 0 = gerade (n-1)
    | otherwise = ungerade (-n)
