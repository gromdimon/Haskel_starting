-- Informatik A WiSe 20/21
-- 17.01.2021
-- Max Willert
module BinSuchBaeume where

data BinBaum a = Nil | Knoten a (BinBaum a) (BinBaum a) deriving (Show,Eq)

baum = Knoten 12
           (Knoten 7 
                   (Knoten 4 Nil Nil)
                   (Knoten 9 Nil (Knoten 11 Nil Nil))
           )
           (Knoten 20
                   (Knoten 15 Nil (Knoten 17 (Knoten 16 Nil Nil) Nil))
                   (Knoten 28 Nil (Knoten 30 Nil Nil))
           )

-- Suchen ------------------------------------------------------------
-- Vor.: Der eingegebene Baum erfüllt die BSB-Eigenschaft.
-- Erg.: True ist genau dann geliefert, wenn x im Baum einthalten ist.
suchen :: Ord a => a -> BinBaum a -> Bool
suchen _ Nil = False
suchen x (Knoten y lub rub)
    | x == y = True
    | x < y = suchen x lub
    | otherwise = suchen x rub

-- Einfügen ----------------------------------------------------------
-- Vor.: Der eingegebene Baum erfüllt die BSB-Eigenschaft.
-- Erg.: Eingegebener Baum, in den das Element x
--       eingefügt ist, ist geliefert. Die Ausgabe erfüllt die
--       BSB-Eigenschaft.
einfuegen :: Ord a => a -> BinBaum a -> BinBaum a
einfuegen x Nil = Knoten x Nil Nil
einfuegen x (Knoten y lub rub)
    | x <= y = Knoten y (einfuegen x lub) rub
    | otherwise = Knoten y lub (einfuegen x rub)

-- Löschen -----------------------------------------------------------

-- Vor.: Der eingegebene Baum ist nicht leer und erfüllt die
--       BSB-Eigenschaft.
-- Erg.: Das minimale Element des Baums ist geliefert.
-- Idee des Algorithmus: Wir erhalten das Minimum, wenn wir im Baum
-- so weit wie möglich nach links gehen.
minTree :: Ord a => BinBaum a -> a
minTree Nil = error"Es gibt kein minimales Element"
minTree (Knoten y Nil _) = y
minTree (Knoten _ lub _) = minTree lub

-- Vor.: Der eingegebene Baum erfüllt die BSB-Eigenschaft und nicht leer.
-- Erg.: Die Wurzel des Baums ist entfernt und die BSB-Eigenschaft ist
--       weiterhin erfüllt.
-- Idee des Algorithmus: Hat die Wurzel nur ein Kind, so ist der Teilbaum
-- das Ergebnis. Ansonsten ist das Minimum des rechten Unterbaums ein passender
-- Kandidat für die Wurzel. Wir fügen diese dort ein und löschen das Minimum
-- im rechten Unterbaum (rekursiv).
loescheWurzel :: Ord a => BinBaum a -> BinBaum a
loescheWurzel Nil = error"Es gibt keine Wurzel"
loescheWurzel (Knoten _ lub Nil) = lub
loescheWurzel (Knoten _ Nil rub) = rub
loescheWurzel (Knoten _ lub rub) = Knoten m lub (loesche m rub)
    where m = minTree rub

-- Vor.: Der eingegebene Baum erfüllt die BSB-Eigenschaft.
-- Erg.: Eingegebener Baum, in dem ein Vorkommen des Elements x
--       entfernt ist, ist geliefert. Die Ausgabe erfüllt die
--       BSB-Eigenschaft. Ist x nicht enthalten, so ist der
--       Ausgabebaum gleich dem Eingabebaum
-- Lösche ein gegebenes Element x aus dem Baum
-- Aus dem leeren Baum braucht man nichts löschen.
-- Falls x kleiner als die Wurzel ist, lösche x rekursiv im linken
-- Unterbaum. Falls das Element größer als die Wurzel, lösche es 
-- rekursiv im rechten Unterbaum. Ansonsten lösche die Wurzel.
loesche :: Ord a => a -> BinBaum a -> BinBaum a
loesche _ Nil = Nil
loesche x b@(Knoten y lub rub)
   | x < y = Knoten y (loesche x lub) rub
   | x > y = Knoten y lub (loesche x rub)
   | otherwise = loescheWurzel b