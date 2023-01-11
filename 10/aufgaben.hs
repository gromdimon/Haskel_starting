-- Aufgabe 10
module Aufgabe10 where
import Data.List

--1. Gringotts
{- Die Zaubererbank Gringotts will mit der Zeit gehen und auf ein Computersystem um- steigen. 
Es gibt Knuts, Sickel und Galleonen. 29 Knuts sind ein Sickel und 17 Sickel sind eine Galleone.
 
(a) Definiere ein Typsynonym fu ̈r Knuts, Sickel und Galleonen. Schreibe außerdem einen 
algebraischen Datentypen zauberGeld, welches einen Preis in der Zauber- wa ̈hrung repra ̈sentiert, 
bestehend aus Knuts, Sickel und Galleonen. -}

type Knuts = Int
type Sickel = Int
type Galleonen = Int

data ZauberGeld = ZauberGeld Knuts Sickel Galleonen deriving (Show, Eq)

{- (b) Die Goblins brauchen eine Mo ̈glichkeit, zu u ̈berpru ̈fen, ob ein Kunde die Summe, die er:sie 
abheben mo ̈chte, tatsa ̈chlich besitzt. (Zum Beispiel mo ̈chte Ms. Granger gerne 2 Galleonen abheben. 
In ihrem Verlies sind jedoch nur 30 Sickel und 123 Knuts. Kann sie die gefragte Menge abheben? 
Wie viel hat sie danach noch in ihrem Verlies?)

U ̈berlege dir, wie diese Differenz sinnvoll repra ̈sentiert werden kann und schreibe eine Funktion 
differenz, welche die Differenz zwischen zwei eingegebenen Preisen, bestehend aus Knuts, Sickel 
und Galleonen, berechnet. Definiere dazu sinnvolle Hilfsfunktionen und erkla ̈re, warum deine Wahl 
sinnvoll ist. -}

-- Input ZauberGeld and ZauberGeld
-- Output ZauberGeld, auch negative!
differenz :: ZauberGeld -> ZauberGeld -> ZauberGeld
differenz (ZauberGeld k1 s1 g1) (ZauberGeld k2 s2 g2) = ZauberGeld k3 0 0
    where 
        zuKnuts (ZauberGeld k s g) = k + s * 29 + g * 493
        k3 = (zuKnuts (ZauberGeld k1 s1 g1)) - (zuKnuts (ZauberGeld k2 s2 g2))

        
{- Tests:
ghci> z1 = ZauberGeld 10 20 30
ghci> z2 = ZauberGeld 20 30 40
ghci> differenz z1 z2
-5230
ghci> differenz (ZauberGeld 10 20 30) (ZauberGeld 20 10 10)
10140
-}


{- (c) Schreibe eine Funktion kannAbheben, die bei U ̈bergabe eines Betrags und ei- nes Kontostandes 
zuru ̈ck gibt, ob die Transaktion valide ist, also ob ein Kunde genu ̈gend Geld besitzt. -}

-- Input Konto1 Konto2
-- Output if Konto2 has more or equal money
kannAbheben :: ZauberGeld -> ZauberGeld -> Bool
kannAbheben (ZauberGeld k1 s1 g1) (ZauberGeld k2 s2 g2) = zuKnuts (ZauberGeld k1 s1 g1) <= zuKnuts (ZauberGeld k2 s2 g2)
    where 
        zuKnuts (ZauberGeld k s g) = k + s * 29 + g * 493

{- Tests:
ghci> kannAbheben (ZauberGeld 10 20 30) (ZauberGeld 20 10 10)
False
ghci> kannAbheben (ZauberGeld 10 0 3) (ZauberGeld 1 1 2)
False
ghci> kannAbheben (ZauberGeld 10 10 3) (ZauberGeld 1 1 2)
False
ghci> kannAbheben (ZauberGeld 10 10 3) (ZauberGeld 10 100 100)
True
ghci> differenz  (ZauberGeld 10 10 3) (ZauberGeld 10 100 100)
ZauberGeld 0 0 (-50431)
-}


{- (d) Viele Zauberer mo ̈gen es nicht, viele Mu ̈nzen bei sich zu haben. Manche gehen daher ha ̈ufig 
zur Bank, um ihre Mu ̈nzen umtauschen zu lassen.
Schreibe eine Funktion tausche, welche einen Betrag, bestehend aus Knuts, Sickel und Galleonen, 
bekommt und einen neuen Betrag zuru ̈ckgibt, der ebenfalls aus Knuts, Sickel und Galleonen besteht, 
denselben Wert wie der urspru ̈ngliche Betrag hat aber aus so wenig Mu ̈nzen wie mo ̈glich besteht. -}

-- Input ZauberGeld only Int >= 0
-- Output ZauberGeld
tausche :: ZauberGeld -> ZauberGeld
tausche (ZauberGeld k s g) = (ZauberGeld (k`mod`493) (s`mod`17+ k`div`29`mod`17) (g + s`div`17 + k`div`493))

{- Tests: 
ghci> tausche (ZauberGeld 493 18 0)
ZauberGeld 0 1 2
ghci> tausche (ZauberGeld 493 18 2)
ZauberGeld 0 1 4
ghci> tausche (ZauberGeld 494 15 2)
ZauberGeld 1 15 3
-}


-- 2. Algebraischer Datentyp Menge
{- In der Lerneinheit hatten wir den Datentyp Liste definiert. Wir definieren jetzt einen
Datentyp, der eine Menge darstellt. 
data Menge a = Menge [a]

(a) Deklariere den Typ Menge als Instanz der Typklasse Show. Implementiere dazu die Funktion
show :: Show a => Menge a -> String
Zum Beispiel soll die Ausgabe einer Menge Menge [1,5,6] das Format {1,5,6}
haben.
-}

data Menge a = Menge [a]

instance Show a => Show (Menge a) where
    show (Menge a) = "{" ++ (intercalate "," (map show a)) ++ "}"

{- Tests: 
ghci> m = Menge[1,2,3]
ghci> m
{1,2,3}
ghci> n = Menge['a', 'b']
ghci> n
{'a','b'}
-}

{- (b) Implementiere die Funktion
vonListe :: Eq a => [a] −> Menge a
die aus einer Liste vom Typ a eine Menge erstellt. Dabei sollen Duplikate aus der Liste entfernt 
werden. Beispielsweise soll vonListe [1,2,2,3,1] das Ergebnis Menge [1,2,3] zuru ̈ckgeben. 
Im Folgenden ko ̈nnt ihr davon ausgehen, dass alle Werte des Typs Menge durch die Funktion vonListe 
erzeugt wurden. -}

vonListe :: Eq a => [a] -> Menge a
vonListe [] = Menge []
vonListe (x:xs) = Menge (newlist (x:xs))
    where
        newlist [] = []
        newlist (x:xs) = x : newlist (filter (/=x) xs)

{- Tests:
ghci> vonListe [1,2,2]
{1,2}
ghci> vonListe [1,2,3,4,5,2]
{1,2,3,4,5}
-}

{- (c) Deklariere den Typ Menge als Instanz der Typklasse Eq. Implementiere dazu die Funktion
(==) :: Eq a => Menge a −> Menge a −> Bool
Dabei soll Menge xs == Menge ys genau dann wahr sein, wenn xs und ys diesel- ben Elemente enthalten 
(die Reihenfolge spielt dabei keine Rolle!). Beispielsweise soll Menge [1,5,2] == Menge [2,1,5] 
wahr sein, wa ̈hrend Menge [1,5,2] == Menge [1,5] falsch ist. -}

instance Eq a => Eq (Menge a) where
    (Menge xs) == (Menge ys) = inList xs ys && inList ys xs
        where
            inList [] _ = True
            inList (x:xs) ys = x `elem` ys && inList xs ys

{- Tests:
ghci> m1 = Menge[1,5,2]
ghci> m2 = Menge[2,1,5]
ghci> m1 == m2
True
ghci> m3 = Menge[1,2]
ghci> m1 == m3
False
-}

{-(d) Implementiere die beiden Funktionen
union, intersection :: Eq a => Menge a −> Menge a −> Menge a
welche die Vereinigung und den Schnitt zweier Mengen darstellen. Die Funktionen vonListe, filter 
und elem ko ̈nnen hier helfen.
-}

unionn :: Eq a => Menge a -> Menge a -> Menge a
unionn (Menge xs) (Menge ys) = vonListe (inList xs ys)
        where
            inList [] _ = []
            inList (x:xs) ys 
                | x `elem` ys = x : inList xs ys
                | otherwise = inList xs ys