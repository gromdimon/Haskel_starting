die_eine_Liste :: [Int]
die_eine_Liste = [3, 4, 5, 2, 3, 3, 4]

{-(c) Trisort funktioniert wie folgt: Die Grundlage ist Mergesort, allerdings wird die
Liste nicht in zwei sondern in drei etwa gleich große Teile zerlegt. Zwei von den
drei Teillisten werden rekursiv mit Mergesort sortiert und die dritte Teilliste wird
mit Insertsort sortiert. Anschließend werden die drei sortierten Teillisten im Merge-
Schritt zu einer sortierten Liste zusammengef ̈ugt. Implementiere Sie Trisort.
Ein sinnvolles Testprogramm k ̈onnte wie folgt aussehen:
liste = map sin [1.0, 2.0 .. 1000.0] −− Liste mit 1000 Sinus−Werten
testliste
| mergesort liste == trisort liste = ”Korrekt”
| otherwise = ” Nicht korrekt”-}

triSort :: Ord a => [a] -> [a]
triSort [] = []
triSort [x] = [x]
triSort xs = merge (merge (mergeSort first) (mergeSort second)) (insertSort third)
        where (first, second, third) = triple xs

triple :: Ord a => [a] -> ([a],[a],[a])
triple xs = (f,s,t)
    where
        (part, t) = halbiere xs
        (f,s) = halbiere part


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
