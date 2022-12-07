-- Aufgebenblatt 7 zur Einheit Listen
-- Autors:  (Name, Matrikelnummer)
--          (Name, Matrikelnummer)
-- Datum:  2014-12-01



-- Aufgabe 1 Welch eine List(e)
{- (a) Schreibe eine Funktion sumdv, die eine Liste mit ganzen Zahlen erha ̈lt und alle Zahlen
der Liste aufaddiert, die durch 3 (daher das d) oder 4 (daher das v) aber nicht durch 6 teilbar sind. 
Die Funktionen sum, filter, Faltungen sowie Listenge- neratoren sind verboten. -}

sumdv :: [Int] -> Int
sumdv [] = 0
sumdv (x:xs) = if (mod x 3 == 0 || mod x 4 == 0) && mod x 6 /= 0 then x + sumdv xs else sumdv xs

{- Tests:
> sumdv [1,2,2,3,4,5]
7
> sumdv [3,4]
7
> sumdv [6,12]
0
-}


{-( b) Schreibe eine Funktion zensiert, die einen Character c sowie eine Zeichenkette xs bekommt 
und alle Vorkommen von c in xs durch den Character * ersetzt. Die Funktion map sowie Listengeneratoren 
sind verboten. Testet die Funktion mindestens mit euren beiden Lieblingsflachwitzen. 
Beispiel:
zensiert ‘l‘ "Rollt eine Kugel um die Ecke und faellt hin." 
liefert: "Ro**t eine Kuge* um die Ecke und fae**t hin." -}

zensiert :: Char -> String -> String
zensiert c [] = []
zensiert c (x:xs) = if x == c then '*' : zensiert c xs else x : zensiert c xs

{- Tests:
> zensiert 'f' "funny"
"*unny"
> zensiert 'a' "abracadabra"
"*br*c*d*br*"
> zensiert 'k' "kararoke"
"*araro*e"
-}


{- (c) Implementiere eine Funktion dropTake, die eine Liste xs und eine natu ̈rliche Zahl n bekommt 
und sowohl die ersten n als auch die letzten n Elemente beha ̈lt und alle anderen Elemente wegschmeißt. 
Gibt es weniger ho ̈chstens 2n Elemente, werden alle Elemente behalten. 
Beispiele:
dropTake 3 [0,1,2,3,4,5,6,7,8,9] => [0,1,2,7,8,9] 
dropTake 4 [0,1,2,3,4,5,6] => [0,1,2,3,4,5,6] 
dropTake 0 [0,1,2,3,4,5,6] => []
-}

dropTake :: Int -> [a] -> [a]
dropTake n xs = take n xs ++ drop (length xs - n) xs

{- Tests:
> dropTake 1 [1,2,3,4,5,6,7,8,9]
[1,9]
> dropTake 4 [1,2,3,4,5,6,7,8,9]
[1,2,3,4,6,7,8,9]
> dropTake 7 [1,2,3,4,5,6,7,8,9]
[1,2,3,4,5,6,7,3,4,5,6,7,8,9]
> dropTake 17 [1,2,3,4,5,6,7,8,9]
[1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9]
> dropTake 0 [1,2,3,4,5,6,7,8,9]
[]
-}



-- Aufgabe 2 MapMapMapMapM ̈apMo ̈pMo ̈o ̈p
{- (a) Implementiere eine Funktion lowhigh, die eine Liste xs mit ganzen Zahlen erha ̈lt 
und zwei natu ̈rliche Zahlen liefert: Die eine Zahl gibt an, wie viele Elemente der Liste 
negativ waren und die zweite Zahl gibt an, wie viele Elemente der Liste positiv waren. 
Beispiel:
lowhigh [−2,3,0,−4,1,0,4] => (2,3) -}

lowhigh :: [Int] -> (Int, Int)
lowhigh xs = (length (filter (<0) xs), length (filter (>0) xs))

{- Tests:
-}

{- (b) Implementiere eine Funktion lowhighFunc, die ebenfalls eine Liste xs mit ganzen Zahlen erha ̈lt 
und zwei natu ̈rliche Zahlen liefert. Dieses Mal wird jedoch noch eine Funktion f : Int -> Int 
u ̈bergeben. Die erste Ausgabezahl gibt an, wie viele Elemente der Liste die Funktion negativ machen 
und die zweite Ausgabezahl gibt an wie viele Elemente der Liste die Funktion positiv machen. 
Verwende die Funktion map. 
Beispiel:
f x = x*x−5
lowhighFunc f [−2,3,0,−4,1,0,4] => (4,3)
 -}

lowhighFunc :: (Int -> Int) -> [Int] -> (Int, Int)
lowhighFunc f xs = (length (filter (<0) (map f xs)), length (filter (>0) (map f xs)))

funcTest :: Int -> Int
funcTest x = x^2 -5

{- Tests:
-}

{- (c) Implementiere eine Funktion absmin, welche eine Liste mit Gleitkommazahlen bekommt 
und den Betrag derjenigen Zahl liefert, die den kleinsten Abstand zur 0 hat. 
Verwende die map-Funktion.
absmin [1.6 , −0.1 , −7.0 ,2.2] => 0.1
 -}

absmin :: [Float] -> Float
absmin xs = minimum (map abs xs)

{- Tests:
-}

{- (d) Implementiere ein Funktion toBools, die eine Liste xs mit Elementen beliebigen Typs sowie 
ein Pra ̈dikat p :: a -> Bool bekommt und all Elemente der Liste gema ̈ß des Pra ̈dikats p 
in True oder False umwandelt. Verwende die Funktion map.
toBools (<2) [ −4 ,2 ,3 ,0 ,1] => [True, False , False , True, True] -}

toBools :: (a -> Bool) -> [a] -> [Bool]
toBools p xs = map p xs

predikatTest :: Int -> Bool
predikatTest x = x < 3

{- Tests: 
> toBools predikatTest [1,2,3,4,5,6,7]
[True,True,False,False,False,False,False]
> toBools predikatTest [-3,-4]
[True,True]
> toBools predikatTest [0,3,16660]
[True,False,False]
-}


-- Aufgabe 3 Herons roots (ohne Punkte)
{- Let a be a floating point number. Implement the algorithm of Heron to compute at least 10 
decimal places of the root of a. Moreover, always give the number of steps that were necessary 
to compute these digits. -}

heron :: Float -> (Float, Int)
heron num = (heronHelper num 1 0, heronHelper num 1 1)

heronHelper :: Float -> Float -> Int -> Float
heronHelper num x steps
    | steps == 10 = x
    | otherwise = heronHelper num ((x + num/x)/2) (steps+1)

{- Tests:
> heron 2
(1.4142135,10)
> heron 3
(1.7320508,10)
> heron 4
(2.0,10)
-}
