-- UB 11
module Aelf where

import Data.Char

{- 2. Funktionen auf Bin ̈arb ̈aumen

Gegeben sei der folgende Algebraische Datentyp fu ̈r (unechte) Bina ̈rba ̈ume:
   data BinBaum a = Nil | Knoten a (BinBaum a) (BinBaum a)
Mit Hilfe von 'foldr einfuegen Nil xs' ko ̈nnt ihr ganz schnell einen Bina ̈ren Suchbaum fu ̈r Testzwecke 
erstellen, ihr mu ̈sst nur eine konkrete Liste xs angeben. Wir wollen uns in (a) und (b) mit Ba ̈umen 
bescha ̈ftigen, deren Knoten Zeichenketten enthalten. -}

data BinBaum a = Nil | Knoten a (BinBaum a) (BinBaum a) deriving (Show,Eq)
testBaum1 = Knoten ("a") (Knoten ("b") Nil Nil) (Knoten ("c") Nil Nil)
testBaum2 = Knoten ("a") (Knoten ("b") Nil Nil) (Knoten ("c") Nil (Knoten ("d") Nil Nil))
testIntBaum = Knoten (2) (Knoten (3) Nil Nil) (Knoten (1) Nil Nil)

{- (a) Implementiere eine Funktion contains, die u ̈berpru ̈ft, ob ein Symbol in irgendeiner Zeichenkette 
des Baums enthalten ist. Implementiere außerdem eine Funktion longest, welche die la ̈ngste Zeichenkette 
des Baums ausgibt. Die Signaturen sehen wie folgt aus:
       contains :: Ord a => BinBaum String -> Char -> Bool
       longest :: BinBaum String -> String -}

contains :: BinBaum String -> Char -> Bool
contains Nil _ = False
contains (Knoten s l r) c = c `elem` s || contains l c || contains r c

{- Tests:
ghci> contains Nil 'a'
False
ghci> contains testBaum1 'a'
True -}

longest :: BinBaum String -> String
longest Nil = []
longest (Knoten s l r) = max (max (longest l) (longest r)) s

{- Tests:
ghci> longest testBaum1
"c"
ghci> longest testBaum2
"d"
ghci> longest Nil
"" -}

{- (b) Implementiere eine Funktion histogramm, die fu ̈r jeden Char, der in den Strings des Baums auftaucht, 
angibt, wie oft der jeweilige Char im Baum vorkommt. Implementiere außerdem eine Funktion chiffre, die jeden 
String des Baums gema ̈ß der Ca ̈sar-Kodierung verschlu ̈sselt. Nutze die mapT-Funktion (letztes Video der 
Einheit). Die Signaturen sehen wie folgt aus:
       histogramm :: BinBaum String -> (Char, Int)
       chiffre :: Int -> BinBaum String -> BinBaum String -}

mapT :: (a -> b) -> BinBaum a -> BinBaum b
mapT f Nil = Nil
mapT f (Knoten x l r) = Knoten (f x) (mapT f l) (mapT f r)

{- Tests:
ghci> mapT reverse testBaum1
Knoten "a" (Knoten "b" Nil Nil) (Knoten "c" Nil Nil)
ghci> mapT reverse testBaum2
Knoten "a" (Knoten "b" Nil Nil) (Knoten "c" Nil (Knoten "d" Nil Nil))
ghci> mapT reverse Nil
Nil -}

histogramm :: BinBaum String -> [(Char, Int)]
histogramm Nil = []
histogramm (Knoten s l r) = (histogramm l) ++ (histogramm r) ++ (histogramm' s)

histogramm' :: String -> [(Char, Int)]
histogramm' [] = []
histogramm' (x:xs) = (x, 1 + length (filter (==x) xs)) : histogramm' (filter (/=x) xs)

{- Tests:
ghci> histogramm testBaum1
[('b',1),('c',1),('a',1)]
ghci> histogramm Nil
[] -}

chiffre :: Int -> BinBaum String -> BinBaum String
chiffre n = mapT (map (\c -> chr (ord c + n)))

{- Tests:
ghci> chiffre 2 testBaum1
Knoten "c" (Knoten "d" Nil Nil) (Knoten "e" Nil Nil)
ghci> chiffre 1232 testBaum1
Knoten "\1329" (Knoten "\1330" Nil Nil) (Knoten "\1331" Nil Nil) -}

{- (c) Die folgende Definition entspricht der Faltung von Ba ̈umen:
       foldt :: (a -> b -> b -> b) -> b -> BinBaum a -> b
       foldt f e Nil = e
       foldt f e (Knoten x lub rub) = f x (foldt f e lub) (foldt f e rub)
Benenne Gemeinsamkeiten und Unterschiede zur Rechtsfaltung von Listen. Beziehe hierbei sowohl die Signatur 
als auch die Funktionsdefinition mit ein. -}

{- Gemeinsamkeiten: 
   - beide sind rekursiv
   Unterschiede:
   - ghci> :t foldt
     foldt :: (a -> b -> b -> b) -> b -> BinBaum a -> b
     ghci> :t foldr
     foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
   - foldt hat eine andere Signatur (logisch, da foldt auf BinBaum a und nicht auf [a] arbeitet)
   - foldt hat eine andere Rekursionsregel (da Spezifikation anders ist -}

{- (d) Implementiere die Funktionen hoehe, anzahl und sumT (siehe letztes Video der Einheit) mit Hilfe der 
Baumfaltung. Beachte, dass der Operator (+) zwei Eingaben bekommt, die Funktion f aus der Baumfaltung jedoch 
drei Eingabewerte beno ̈tigt. Das heißt mit sumT = foldt (+) 0 ist es noch nicht getan. -}

foldt :: (a -> b -> b -> b) -> b -> BinBaum a -> b
foldt f e Nil = e
foldt f e (Knoten x lub rub) = f x (foldt f e lub) (foldt f e rub)

hoehe :: BinBaum a -> Int
hoehe = foldt (\_ l r -> 1 + max l r) 0

{- Tests:
ghci> hoehe testBaum1
2
ghci> hoehe testBaum2
3
ghci> hoehe Nil
0 -}

anzahl :: BinBaum a -> Int
anzahl = foldt (\_ l r -> 1 + l + r) 0

{- Tests:
ghci> anzahl testBaum1
3
ghci> anzahl testBaum2
4 -}

sumT :: BinBaum Integer -> Int
sumT Nil = 0
sumT (Knoten x l r) = 1 + anzahl l + anzahl r

{- Tests:
ghci> sumT testIntBaum 
3 -}

{- (e) Implementiere die Inorder-, Postorder- und Preorder-Traversierungen mit Hilfe der Baumfaltung. -}

inorder :: BinBaum a -> [a]
inorder = foldt (\x l r -> l ++ [x] ++ r) []

{- Tests:
ghci> inorder testBaum1
["b","a","c"]
ghci> inorder testBaum2
["b","a","c","d"] -}

postorder :: BinBaum a -> [a]
postorder = foldt (\x l r -> l ++ r ++ [x]) []

{- Tests:
ghci> postorder testBaum1
["b","c","a"]
ghci> postorder testBaum2
["b","d","c","a"] -}

preorder :: BinBaum a -> [a]
preorder = foldt (\x l r -> [x] ++ l ++ r) []

{- Tests:
ghci> preorder testBaum1
["a","b","c"]
ghci> preorder testBaum2
["a","b","c","d"] -}
