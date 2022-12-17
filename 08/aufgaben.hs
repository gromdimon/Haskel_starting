--Aufgebenblatt 8 zur Einheit 08
--Hramyka, D. und Tiesler, A. Tutorium: Do.8-10 mit Manuel Zschäbitz 
-- 14.12.22
module Aacht where 
-- import Sortieren

import Data.Char

-- 1. <Hier witzigen Aufgabentitel einfu ̈gen>
{-(a) Recherchiere die Funktionen foldr, foldl, foldr1 und foldl1. Erkla ̈re ihre Funktionsweise 
und ihre Signatur. Gib zu jeder Faltung mindestens ein Beispiel und zeige schrittweise, 
wie die Faltung das Ergebnis berechnet.-}

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
Die Funktion nimmt b und letztes Element der Liste und gibt Funktion argument (a -> b -> b) als Ergebnis.
Danach wird Funktion auf Ergebnis und vorletztes Element angewendet und so weiter. Dann liefert es Finalergebnis.
-- Input: foldr (/) 2 [8,12]
-- Output: 1.3333333333333333
-- Lösung: 12/(8/2=6.0) = 1.3333333333333333   
-- Zuerst 8 / 2, dann 12 / 6.0
-- Input: foldr max 111 [3,6,12,4,55,11]
-- Output: 111
-- Lösung: max 3 (max 6 (max 12 (max 4 (max 55 (max 11 111 = 111)=111)=111)=111)=111) = 111
-- Anwenden max auf jedes Element 

foldl :: (b -> a -> b) -> b -> [a] -> b
Die Funktion nimmt b und erstes Element der Liste und gibt Funktion argument (b -> a -> b) als Ergebnis.
Danach wird Funktion auf Ergebnis und zweites Element angewendet und so weiter. Dann liefert es Finalergebnis.
-- Input: foldl (/) 64 [4,2,4]
-- Output: 2.0
-- Lösung: 64/4=16.0, 16.0/2=8.0, 8.0/4=2.0
-- Input: foldl max 111 [3,6,12,4,55,11]
-- Output: 111
-- Lösung: max (max (max (max (max 111 3=111) 6=111) 12=111) 4=111) 55=111) 11=111) = 111

foldr1 :: (a -> a -> a) -> [a] -> a
Die Funktion nimmt erstes Element der Liste und gibt Funktion argument (a -> a -> a) als Ergebnis.

-}



{-(b) Implementiere eine Funktion dez2bin, die eine natu ̈rliche Zahl bekommt und die Bina ̈rdarstellung 
der Zahl als Zeichenkette liefert. Implementiere außerdem die Umkehrfunktion bin2dez. 
Verwende fu ̈r mindestens eine der beiden Funktionen einen Faltung.
dez2bin 42 => ”101010” bin2dez ”101010” => 42-}

--Voraussetzung: Dezimalzahl in eine Binärzahl umwandeln
--Ergebnis: Dezimalzahl in eine Binärzahl umgewandelt
intToChar :: Int -> String
intToChar x
    | x == 0 = ['0']
    | x == 1 = ['1']
    | x == 2 = ['2']
    | x == 3 = ['3']
    | x == 4 = ['4']
    | x == 5 = ['5']
    | x == 6 = ['6']
    | x == 7 = ['7']
    | x == 8 = ['8']
    | x == 9 = ['9']
    | x > 9 = (intToChar (x `div` 10)  ++ intToChar (mod x 10 ) )

-- Voraussetzung: a ist natuerliche Zahl und Funktion ist immer intToChar
-- Ergebnis: String von binaere Darstellung von a
-- Funktion macht Binaerdarstellung von dezimale Zahl durch teilen durch 2
dez2bin :: (Int -> String) -> Int -> String
dez2bin f 0 = "0"
dez2bin f num = dez2bin f (num `div` 2) ++ f (num `mod` 2)

--Voraussetzung: Immer intToChar Funktion angeben und ganzes Binaerzahl
--Ergebnis: Binärzahl in eine Dezimalzahl umgewandelt
bin2dez :: String -> Int
bin2dez [] = 0
bin2dez xs = foldl1 f (map (0-48+) (map (ord) xs)) where
        f p x = x + (p*2)
-- Wenn es wichtig ist String am Ende zu haben, verwende intToChar Funktion auf Ergebnis!



{-(c) Gegeben seien die beiden unten stehenden Datentypen. Implementiere eine Funktion maxEintrag, 
die eine Tabelle bekommt und einen Eintrag liefert, der einen gro ̈ßten Int-Wert besitzt. 
Nutze eine Faltung
type Eintrag = (Char, Int)
type Tabelle = [ Eintrag ]-}

--Voraussetzung: Tabelle mit Einträgen
--Ergebnis: Eintrag mit dem größten Int-Wert
type Eintrag = (Char, Int)
type Tabelle = [ Eintrag ]
maxEintrag :: Tabelle -> Eintrag
maxEintrag [] = (' ', 0)
maxEintrag (x:xs) = foldl (\x y -> if (snd x) > (snd y) then x else y) x xs

{-Test:
> maxEintrag [('a',2), ('c',3), ('d',6)]
('d',6)-}



{-(d) Implementiere eine Funktion
megaMap :: Int −> (a −> a) −> [a] −> [a]
welche a ̈hnlich wie map eine Funktion f und eine Liste xs erha ̈lt. Zusa ̈tzlich bekommt megaMap 
noch eine natu ̈rliche Zahl n u ̈bergeben. Die Funktion megaMap wendet auf jedes Element der 
Eingabeliste n Mal die Funktion f an.-}

--Voraussetzung: Liste, Funktion und natürliche Zahl
--Ergebnis: Liste mit n mal angewandter Funktion
megaMap :: Int -> (a -> a) -> [a] -> [a]
megaMap 0 f xs = xs
megaMap num f xs = megaMap (num-1) f (map f xs)

{-Test:
> megaMap 3 (+2) [1,2,3]
[7,8,9]
> megaMap 10 (^2) [1,2,3]
[1,179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216,373391848741020043532959754184866588225409776783734007750636931722079040617265251229993688938803977220468765065431475158108727054592160858581351336982809187314191748594262580938807019951956404285571818041046681288797402925517668012340617298396574731619152386723046235125934896058590588284654793540505936202376547807442730582144527058988756251452817793413352141920744623027518729185432862375737063985485319476416926263819972887006907013899256524297198527698749274196276811060702333710356481]-}



{-(e) Betrachte die folgende Funktion:
foo :: [a] −> [a]
foo xs = foldl (flip (:)) [] xs
Zeige die einzelnen Schritte der Berechnung foo [1,2,3]. Dazu muss die Bedeutung der Funktion flip 
recherchiert werden: gib deren Signatur und Spezifikation an. Stelle außerdem eine Vermutung auf, 
was die Funktion foo macht.
-}
-- Voraussetzung: Liste
-- Ergebnis: Liste mit applied Funktion
-- Signatur foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- Signatur flip :: (a -> b -> c) -> b -> a -> c
foo :: [a] -> [a]
foo xs = foldl (flip (:)) [] xs 
{-
foo [1,2,3]
= foldl (flip (:)) [] [1,2,3]
= foldl (flip (:)) [] (1:(2:(3:[])))
= foldl (flip (:)) (flip (:) 1 []) (2:(3:[]))
and so on...
= foldl (flip (:)) (flip (:) 1 (flip (:) 2 [])) (3:[])
= foldl (flip (:)) (flip (:) 1 (flip (:) 2 (flip (:) 3 []))) []
= foldl (flip (:)) (flip (:) 1 (flip (:) 2 (3:[]))) []
= foldl (flip (:)) (flip (:) 1 (2:(3:[]))) []
= foldl (flip (:)) (1:(2:(3:[]))) []
= (1:(2:(3:[])))
-}



--- 2. Immer diese Sortiererei 
{-(a) Schreibe eine allgemeine Funktion remove x xs, welche das erste Vorkommen von x in xs entfernt. 
Gib eine Signatur an. (Die Funktion remove1 aus der Datei extras falten.hs lo ̈st schon fast 
das Problem, sie muss nur leicht angepasst werden.)
remove 6 [4,3,6,7,2,6,9] => [4,3,7,2,6,9]-}

--Voraussetzung: Element und Liste
--Ergebnis: Liste ohne erstes Vorkommen von Element
remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys) = if x == y then ys else y : remove x ys
{-Tests:
> remove 'g' "gaga"
"aga"
> remove 'm' "amin"
"ain"-}



{-(b) Wende Mergesort und Insertsort auf die folgende Liste an und zeige die einzelnen Schritte: 
[1,6,2,7,9,0,4,2,9]-}

{-
Insertsort:
[insert 1,insert 6,insert 2,insert 7,insert 9,insert 0,insert 4,insert 2,insert 9] insert []
[insert 1,insert 6,insert 2,insert 7,insert 9,insert 0,insert 4,insert 2,insert 9] ++ []
[insert 1,insert 6,insert 2,insert 7,insert 9,insert 0,insert 4,insert 2] ++ [9]
[insert 1,insert 6,insert 2,insert 7,insert 9,insert 0,insert 4] ++ [2,9]
[insert 1,insert 6,insert 2,insert 7,insert 9,insert 0] ++ [2,4,9]
...
[0,1,2,2,4,6,7,9,9]

Mergesort:
split to {  [ (1,6),(2,7) ],  ( [ (9,0),(4,2) ] ,9 )}
vergleiche 1 und 6, 2 und 7, 9 und 0, 4 und 2, 9 und nothing
vergleichen (4,2) und 9
vergleichen (1,6) und (2,7), (9,0) und (4,2,9)
vergleichen (1,2,6,7) und (0,2,4,9,9)
[0,1,2,2,4,6,7,9,9]
-}



{-(c) Trisort funktioniert wie folgt: Die Grundlage ist Mergesort, allerdings wird die Liste nicht 
in zwei sondern in drei etwa gleich große Teile zerlegt. Zwei von den drei Teillisten werden rekursiv 
mit Mergesort sortiert und die dritte Teilliste wird mit Insertsort sortiert. Anschließend werden die 
drei sortierten Teillisten im Merge- Schritt zu einer sortierten Liste zusammengefu ̈gt. Implementiere 
Sie Trisort. Ein sinnvolles Testprogramm ko ̈nnte wie folgt aussehen:
liste = map sin [1.0 ,2.0..1000.0] −− Liste mit 1000 Sinus−Werten 
test liste
    | mergesort liste == trisort liste = ”Korrekt” 
    | otherwise = ”Nicht korrekt”-}


--Voraussetzung: Liste
--Ergebnis: Sortierte Liste
triSort :: Ord a => [a] -> [a]
triSort [] = []
triSort [x] = [x]
triSort xs = merge (merge (mergeSort first) (mergeSort second)) (insertSort third)
        where (first, second, third) = split xs

--Voraussetzung: Liste
--Ergebnis: Liste in drei Teillisten geteilt
split :: [a] -> ([a],[a],[a])
split xs = (take n xs, take m (drop n xs), drop m (drop n xs))
    where n = length xs `div` 3
          m = length xs `div` 3 + n

--  COPY FROM LEKTURES
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


{- Tests:
> triSort [2,3,547,1,8,23,5,9,0,3]
[0,1,2,3,3,5,8,9,23,547]
> triSort [9,8,7,6,5,4,3,2]
[2,3,4,5,6,7,8,9]
-}

{-(d) Der Sortieralgorithmus selectsort funktioniert nach dem Prinzip Smart-Choice- &-Stupid-Insertion:
(i) Finde, merke und entferne ein gro ̈ßtes Element der Liste.
(ii) Sortiere den Rest der Liste rekursiv.
(iii) Fu ̈ge das gemerkte Element an den Anfang der rekursiv sortierten Restliste. 
type Eintrag = (Char, Int)
type Tabelle = [ Eintrag ]
Implementiere selectsort :: Tabelle -> Tabelle, welche eine Tabelle mit Eintra ̈gen bekommt 
und die Eintra ̈ge absteigend nach Int-Wert sortierst Nutze dazu 1.c) und 2.a)-}

--Voraussetzung: Liste
--Ergebnis: Sortierte Liste
selectSort :: Tabelle -> Tabelle
selectSort [] = []
selectSort xs = maxElem xs : selectSort (delete (maxElem xs) xs)

--Voraussetzung: Liste
--Ergebnis: Groesstes Element der Liste
maxElem :: Tabelle -> Eintrag
maxElem [x] = x
maxElem (x:xs)
    | snd x > snd (maxEintrag xs) = x
    | otherwise = maxEintrag xs

--Voraussetzung: Liste
--Ergebnis: Liste ohne das erste Vorkommen von x
delete :: Eintrag -> Tabelle -> Tabelle
delete x [] = []
delete x (y:ys)
    | x == y = ys
    | otherwise = y : delete x ys

{- Tests:
> selectSort [('a',2), ('c',3), ('d',6)]
[('d',6),('c',3),('a',2)]
> selectSort [('a',2), ('c',3), ('d',6), ('g',1922), ('k', 1)]
[('g',1922),('d',6),('c',3),('a',2),('k',1)]-}



{-(e) Analysiere die Laufzeit von Selectsort. Fu ̈r die Laufzeit sollen die Vergleiche geza ̈hlt werden, 
die in der maxEintrag-Funktion auftauchen. (Je nachdem, wie ihr diese implementiert, kann ein Vergleich
auch dem Aufruf der Funktion max entsprechen.)-}

{-
Ich schreibe das, wenn ich sehr stark schlafen moechte, aber ich versuche es trotzdem.
Die Laufzeit von Selectsort ist (n^2), da die Funktion maxElem in jedem Schritt n-mal aufgerufen wird.
Dabei wird in jeder Iteration die Funktion delete aufgerufen, die n-mal aufgerufen wird.
Somit ist die Laufzeit (n^2).
Danke)
-}
