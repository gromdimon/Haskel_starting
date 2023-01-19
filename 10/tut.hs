
-- OnTop c t ist einen Stein der Farbe c auf den Turm t legt
-- PileUp t1 t2 ist den Turm t1 auf t2 stellt



data tree = Node | tree a tree a
data Node a = a
{-
*Main> colorList t1
[Blue]
*Main> colorList t2
[Red]
*Main> colorList t3
[Blue,Red]
*Main> :t t3
t3 :: Tower
*Main> t3
PileUp (OnTop Blue Empty) (OnTop Red Empty)
-}


data Color = Red | Blue deriving (Eq, Show)
data Tower = Empty | OnTop Color Tower  | PileUp Tower Tower deriving Show

colorList :: (Tower a) -> [Color]
colorList (Empty) = []
colorList (OnTop c t) = c : colorList t
colorList (PileUp t1 t2) = colorList t1 ++ colorList t2

instance Eq Tower where
    (==) Empty Empty = True
    (==) t1 t2 = colorList t1 == colorList t2

t3 :: Tower a
t3 = PileUp (OnTop Blue Empty) (OnTop Red Empty)

t4 :: Tower a
t4 = PileUp (OnTop Blue Empty) (OnTop Red Empty)

t5 :: Tower a
t5 = PileUp (OnTop Red Empty) (OnTop Red Empty)