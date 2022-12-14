module Zweiundvierzig where

-- Schreibe Funktionen, die zu folgenden
-- Signaturen passen:

f0 :: Int -> Int -> Int -> Int
f0 x y z = x + y + z

-- Klammerung bei Signaturen ist immer rechtsassoziativ
f1 :: Int -> (Int -> (Int -> Int))
f1 n x y = n + x + y

f2 :: (Int -> Int) -> Int -> Int
f2 f x = f x

f3 :: (Int -> Int) -> (Int -> Int)
f3 f x = f x

-- Vor.: keine
-- Erg.: Eingabeliste, bei dem das Element x zwei Mal vorne rangehangen wurde, ist geliefert.
double :: a -> [a] -> [a]
double x = (x:) . (x:)

-- foldr f e [] = e
-- foldr f e (x:xs) = f x (foldr f e xs)
-- Werte folgenden Ausdruck per Hand aus: foldr double [] (1:(2:[]))

-- Spezifikation?
doubleAll :: [a] -> [a]
doubleAll = foldr double []

-- Vor.: n>=0
-- Erg.: n-fache Komposition der Eingabefunktion ist geliefert.
multi :: Int -> (a -> a) -> (a -> a)
multi 0 f = id
multi n f = f . multi (n-1) f

-- Zeige anhand der Definitionen double x = multi 2 (x:)

-- Spezifikation?
mehrfach :: Int -> [a] -> [a]
mehrfach n = foldr f [] where f x = multi n (x:)

-- Bekannte Funktionen, die mit foldr dargestellt werden können:
-- sum = foldr (+) 0
-- insertsort = foldr insert []
-- histogramm = foldr einsortieren []

-- Vor.: keine
-- Erg.: Ein Liste, in der alle Vorkommen des Eingabeelements
--       entfernt sind, ist geliefert.
remove1, remove2, remove3 :: Eq a => a -> [a] -> [a]
-- Der Zusatz "Eq a" bedeutet, dass nur Datentypen a,
-- für die (==) definiert ist, benutzt werden dürfen.
remove1 c [] = []
remove1 c (x:xs) = if (c==x) then remove1 c xs else x : remove1 c xs

remove2 c = filter (/=c)

remove3 c = foldr f [] where f x ys = if (c==x) then ys else (x:ys)

-- Und was passiert eigentlich hier?
zeros n = take n nullen where nullen = 0 : nullen