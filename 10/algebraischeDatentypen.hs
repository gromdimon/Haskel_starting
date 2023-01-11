-- Informatik A WiSe 20/21
-- 10.01.2021
-- Max Willert
module AlgebraischeDT where

-- Wir haben bereits Typsynonyme gesehen,
-- so z.B. type Eintrag = (Char,Int)
-- Wir wollen aber gänzlich neue Datentypen erschaffen.
-- Dies geschieht mit dem Signalwort 'data'

-- Algebraische Datentypen (kurz: AlDT)
-- Achtung: Algebraischer Datentyp /= ADT

-- 1. Aufzählungen:

data Bit = O | I
data Tag = Mo | Di | Mi | Do | Fr | Sa | So deriving (Show, Eq)
data Wahrheitswert = Falsch | Wahr deriving (Show, Eq)

-- Mithilfe von deriving werden die Datentypen automatische Instanzen
-- der Klassen Show und Eq. Damit produziert der Aufruf von werktag Mi
-- keinen Fehler mehr in ghci oder WinHugs.
-- Für alle Typen in der Typklasse Show ist die Funktion
-- show :: a -> String definiert.
-- Für alle Typen in der Typklasse Eq sind die Funktionen
-- (==), (/=) :: a -> a -> Bool definiert.
-- Für alle Typen in der Typklasse Ord sind die Funktionen
-- compare :: a -> a -> Ordering,
-- (<), (<=), (>=), (>) :: a -> a -> Bool und
-- max, min :: a -> a -> a definiert.

werktag :: Tag -> Wahrheitswert
werktag Sa = Falsch
werktag So = Falsch
werktag _ = Wahr

-- 2. Datentypen mit Werten:

data Adresse = Anschrift String String Int Int String
                   deriving (Eq, Show)

-- 'Anschrift' ist ein Konstruktor. Konstruktoren müssen
-- immer groß geschrieben sein.

gibName :: Adresse -> String
gibName (Anschrift name _ _ _ _) = name

-- mit Record-Schreibweise
data Adresse2 = Anschrift2 {name:: String, strasse::String,
        hausnummer::Int, plz::Int, ort::String} deriving (Show,Eq)

jd = Anschrift2 {name = "JD", strasse = "Takustr.",
                 hausnummer = 9, plz = 14195, ort = "Berlin"}

-- 3. Rekursive Datentypen

data ZahlListe = End | More {wert::Int, rest::ZahlListe} deriving (Eq,Show)

l1 = More 4 (More 6 (More 1 End))

-- allgemeiner mit Typparameter:

data Liste a = Empty | Cons a (Liste a)

l2 = Cons 'a' (Cons 't' (Cons '1' Empty))

-- Da die Darstellung von Listen mithilfe von deriving Show nicht
-- zufriedenstellend ist, wird mithilfe der folgenden Schreibweise
-- der Datentyp Liste a als Instanz der Klasse Show implementiert.

instance Show a => Show (Liste a) where
   show Empty = "[]"
   show (Cons c cs) = show c ++ ":" ++ show cs
   
-- Der Datentyp Liste a wird noch als Instanz der Klassen Eq und Ord
-- implementiert. Die Implementierung von (<=) entspricht der üblichen
-- lexikograpischen Ordnung.
   
instance Eq a => Eq (Liste a) where
   (==) Empty Empty = True
   (==) (Cons x1 xs1) (Cons x2 xs2) = x1 == x2 && xs1 == xs2
   (==) _ _ = False
   
instance Ord a => Ord (Liste a) where
   (<=) Empty _ = True
   (<=) _ Empty = False
   (<=) (Cons x1 xs1) (Cons x2 xs2) = x1 < x2 || x1==x2 && xs1 <= xs2

-- Hier noch ein paar Funktionen die wir von Listen [a] kennen, übersetzt
-- in algebraische Datentypen.

-- map :: (a -> b) -> [a] -> [b]
mapAlDT :: (a -> b) -> Liste a -> Liste b
mapAlDT f Empty = Empty
mapAlDT f (Cons x xs) = Cons (f x) (mapAlDT f xs)

-- (++) :: [a] -> [a] -> [a]
(+++) Empty ys = ys
(+++) (Cons x xs) ys = Cons x ((+++) xs ys)

-- foldr :: (a -> b -> b) -> b -> [a] -> b
foldrAlDT :: (a -> b -> b) -> b -> Liste a -> b
foldrAlDT f e Empty = e
foldrAlDT f e (Cons x xs) = f x (foldrAlDT f e xs)
