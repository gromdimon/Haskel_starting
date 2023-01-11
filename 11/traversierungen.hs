-- Informatik A WiSe 20/21
-- 17.01.2021
-- Max Willert
module Traversierungen where

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

------------------------------------------------------------------------
-- Eine Traversierung ist eine Funktion, die einen Baum in eine Liste
-- von dessen Elementen überführt. Die Spezifikation aller 4 Funktionen
-- lautet daher:

-- Vor.: keine
-- Erg.: Eine Liste mit genau denselben Elementen des Baums ist geliefert.

-- Preorder: Besuche erst die Wurzel, dann den linken Unterbaum, dann
-- den rechten Unterbaum.
preorder :: BinBaum a -> [a]
preorder Nil = []
preorder (Knoten x lub rub) = [x] ++ preorder lub ++ preorder rub

-- Inorder: Besuche erst den linken Unterbaum, dann die Wurzel, dann
-- den rechten Unterbaum.
inorder :: BinBaum a -> [a]
inorder Nil = []
inorder (Knoten x lub rub) = inorder lub ++ [x] ++ inorder rub

-- Postorder: Besuche erst den linken Unterbaum, dann den rechten
-- Unterbaum, dann die Wurzel.
postorder :: BinBaum a -> [a]
postorder Nil = []
postorder (Knoten x lub rub) = postorder lub ++ postorder rub ++ [x]

-- Levelorder: Besuche die Knoten nach aufsteigender Tiefe und innerhalb
-- einer Tiefe von links nach rechts.
levelorder :: BinBaum a -> [a]
levelorder b = hilfe [b] where
    hilfe [] = []
    hilfe (Nil : xs) = hilfe xs
    hilfe ((Knoten x lub rub) : xs) = x : hilfe (xs ++ [lub, rub])
