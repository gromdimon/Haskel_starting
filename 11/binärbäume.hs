-- Informatik A WiSe 20/21
-- 17.01.2021
-- Max Willert
module BinBaeume where

-- Haskell-Definition eines echten Binärbaums
-- (Jeder innere Knoten hat genau 2 Kinder,
-- jedes Blatt hat genau 0 Kinder.)
-- Achtung: Mit dieser Variante ist es nicht möglich, den leeren Baum
-- darzustellen.
data Tree = Leaf | Node Tree Tree deriving (Show,Eq)

b1 = Node (Node Leaf Leaf) Leaf

-- Vor.: keine
-- Erg.: Die Höhe des Binärbaums ist geliefert.
hoehe :: Tree -> Int
hoehe Leaf = 0
hoehe (Node lub rub) = 1 + max (hoehe lub) (hoehe rub)

-- Vor.: keine
-- Erg.: Die Anzahl der inneren Knoten des Binärbaums ist geliefert.
innereKnoten :: Tree -> Int
innereKnoten Leaf = 0
innereKnoten (Node lub rub) = 1 + innereKnoten lub + innereKnoten rub

------------------------------------------------------------------------

-- Unechte Bäume
-- Achtung: Ein Blatt hat hier die Form (NNode NNil NNil).
-- NNil wird als Dummy-Knoten verwendet und entspricht einem leeren Baum.
data NTree = NNil | NNode NTree NTree deriving (Show,Eq)

b2 = NNode (NNode NNil NNil) (NNil)

-- Vor.: keine
-- Erg.: Die Anzahl der Blätter des Binärbaums ist geliefert.
blaetter :: NTree -> Int
blaetter NNil = 0
blaetter (NNode NNil NNil) = 1
blaetter (NNode lub rub) = blaetter lub + blaetter rub

------------------------------------------------------------------------

-- Haskell-Definition eines Binärbaums mit Elementen in den Knoten
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

-- sumItUp berechnet die Summe aller Elemente eines Baumes
sumItUp :: Num a => BinBaum a -> a
sumItUp Nil = 0
sumItUp (Knoten n lub rub) = sumItUp lub + sumItUp rub + n