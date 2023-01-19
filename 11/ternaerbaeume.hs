--Ternaerbaeume by you're fav. Tutis

module Ternaerbaeume where 


data TerTree = Leaf | Node Int TerTree TerTree TerTree

--Die folgenden Funktionen könnt müsst ihr nicht verwenden (für eigene Beispiele)
-------------------------------------------------------------------------------
--Funktion um einen TerTree zu bauen
einfuegen :: Int -> TerTree -> TerTree
einfuegen x Leaf = Node x Leaf Leaf Leaf
einfuegen x (Node y lub mub rub)
    | x < y = Node y (einfuegen x lub) mub rub
    | x < y+y = Node y lub (einfuegen x mub) rub
    | otherwise = Node y lub mub (einfuegen x rub)

-------------------------------------------------------------------------------
--Funktionen um einen TerTree kleiner zu machen
minTree :: TerTree -> Int
minTree Leaf = error"Es gibt kein minimales Element"
minTree (Node y Leaf _ _) = y
minTree (Node _ lub _ _) = minTree lub

loesche :: Int -> TerTree -> TerTree
loesche _ Leaf = Leaf
loesche x b@(Node y lub mub rub)
   | x < y = Node y (loesche x lub) mub rub
   | x < y+y = Node y lub (loesche x mub) rub
   | x > y = Node y lub mub (loesche x rub)
   | otherwise = loescheWurzel b

loescheWurzel :: TerTree -> TerTree
loescheWurzel Leaf = error"Es gibt keine Wurzel"
loescheWurzel (Node _ lub Leaf Leaf) = lub
loescheWurzel (Node _ Leaf mub Leaf) = mub
loescheWurzel (Node _ Leaf Leaf rub) = rub
loescheWurzel (Node _ lub mub rub) = Node m lub (loesche m mub) rub
    where m = minTree mub

----------------------------------------------------------------------------------
---Beispielbaum
baum :: TerTree
baum = Node 12
            (Node 7 
                    (Node 4 Leaf Leaf Leaf)
                    (Node 10 Leaf Leaf Leaf)
                    Leaf
            )
            (Node 20 
                    (Node 15 Leaf Leaf Leaf)
                    (Node 23 
                        (Node 22 Leaf Leaf Leaf) 
                        Leaf 
                        Leaf
                    )
                    Leaf
            )
            (Node 30
                    (Node 25 
                        Leaf
                        (Node 26 Leaf Leaf Leaf)
                        Leaf
                    )
                    (Node 31 
                        Leaf 
                        (Node 32 
                            Leaf 
                            (Node 50 Leaf Leaf Leaf) 
                            Leaf
                        )
                        Leaf
                    )
                    (Node 60 
                        Leaf 
                        (Node 90 Leaf Leaf Leaf) 
                        Leaf
                    )
            )

--ab hier kommt euer Code
--hoehe :: TerTree -> Int


--innereKnoten :: TerTree -> Int


--blaetter :: TerTree -> Int


--suchen :: Int -> TerTree -> Bool


