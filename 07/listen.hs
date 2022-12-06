-- Informatik A WiSe 20/21
-- 14.12.2020
-- Max Willert
module Listen where

-- Listen: Beliebige Anzahl an Werten desselben Datentyps.

-- Formale (induktive) Definition:
--   Sei a ein beliebiger Datentyp. Es gilt
--    - Die leere Liste [] ist vom Typ [a].
--    - Wenn x ein Wert vom Typ a ist, und xs
--      eine Liste vom Typ [a], dann ist auch
--      (x:xs) eine Liste vom Typ [a].
-- Der Doppelpunkt (:) wird als "cons" bezeichnet.
-- Beispiele -> siehe Videos

-- Funktionen über Listen: Wir nutzen das Prinzip des Pattern Matchings.

-- Vor.: keine
-- Erg.: Eine Liste, bei der y vorne an xs gehängt ist, ist geliefert.
vorneAnhaengen :: a -> [a] -> [a]
vorneAnhaengen y xs = y : xs

-- Vor.: keine
-- Erg.: True ist genau dann geliefert, wenn Eingabeliste leer ist.
istLeer :: [a] -> Bool
istLeer [] = True -- Es gibt KEIN erstes Element
istLeer (x:xs) = False  -- Es gibt ein erstes Element

-- Vor.: Eingabeliste enthält mindestens ein Element.
-- Erg.: Das vorderste Element der Liste ist geliefert.
kopf :: [a] -> a
kopf [] = error"Es gibt keinen Kopf!"
kopf (x:xs) = x 

-- Vor.: Eingabeliste enthält mindestens ein Element.
-- Erg.: Die Eingabeliste ohne das erste Element ist geliefert.
rest :: [a] -> [a]
rest [] = error"Geht nicht"
rest (x:xs) = xs

-- Vor.: keine
-- Erg.: Die Anzahl der Elemente der Liste ist geliefert.
laenge :: [a] -> Int
laenge [] = 0
laenge (x:xs) = 1 + laenge xs

-- Vor.: keine
-- Erg.: Die Summe der Elemente der Liste ist geliefert.
--       Ist die Liste leer, so ist 0 geliefert.
summ :: Num a => [a] -> a
summ [] = 0
summ (x:xs) = x + summ xs

-- Vor.: keine
-- Erg.: True ist genau dann geliefert, wenn xs das Element y enthält.
enthaelt :: Eq a => a -> [a] -> Bool
enthaelt y [] = False -- y ist nicht in der leeren Listen
enthaelt y (x:xs)
    | y == x = True -- 1. Fall y ist genau der Kopf der Liste
    | otherwise = enthaelt y xs -- 2. Fall y ist evtl. woanders
	
-- Vor.: Die Eingabeliste beinhaltet mindestens ein Element.
-- Erg.: Ein größtes Element der Liste ist geliefert.
maximumm :: Ord a => [a] -> a
maximumm [] = error"Gibts nicht"
maximumm [x] = x
maximumm (x:xs) = max x (maximumm xs)

-- Vor.: keine
-- Erg.: Eine Liste, bei der ys hinten an xs gehängt ist, ist geliefert.
verbinde :: [a] -> [a] -> [a]
verbinde [] ys = ys
verbinde (x:xs) ys = x : verbinde xs ys

-- Vor.: keine
-- Erg.: Eine Liste, bei der y hinten an xs gehängt ist, ist geliefert.
hintenAnhaengen :: a -> [a] -> [a]
hintenAnhaengen y [] = [y]
hintenAnhaengen y (x:xs) = x : hintenAnhaengen y xs

-- zum Selbst ausprobieren:
-- Vor.: keine
-- Erg.: Die "Paarung" der Einabelisten ist geliefert.
zipp :: [a] -> [b] -> [(a,b)]
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys
zipp _ _ = []

-- Generische Funktionen ----------------------------------------------

-- Ein Datentyp a ist in der Klasse Eq enthalten, wenn zwei beliebige
-- Ausdrücke mit Typ a miteinander auf Gleichheit verglichen werden
-- können. Wir schreiben in der Signatur "Eq a =>" davor, um anzuzeigen,
-- dass NUR Datentypen aus der Klasse Eq verwendet werden dürfen, siehe
-- als Beispiel: enthaelt.
-- Ein Datentyp a ist in der Klasse Ord enthalten, wenn zwei beliebige
-- Ausdrücke mit Typ a miteinander auf bzgl. <, >, <= oder >= verglichen
-- werden können. Wir schreiben in der Signatur "Ord a =>" davor, um
-- anzuzeigen, dass NUR Datentypen aus der Klasse Ord verwendet werden
-- dürfen, siehe als Beispiel: maximumm.
-- Damit Elemente eines Typs a addiert werden können, muss es sich bei a
-- um einen numerischen Datentyp handeln. Schreibweise: "Num a =>".
-- Siehe als Beispiel: summ.
