--Aufgebenblatt 7 zur Einheit 07: Listen
--Hramyka, D. und Tiesler, A. Tutorium: Do.8-10 mit Manuel Zschäbitz 
--Mi. 07.12.22



-- Aufgabe 1 Welch eine List(e)
{- (a) Schreibe eine Funktion sumdv, die eine Liste mit ganzen Zahlen erhält und alle Zahlen
der Liste aufaddiert, die durch 3 (daher das d) oder 4 (daher das v) aber nicht durch 6 teilbar sind. 
Die Funktionen sum, filter, Faltungen sowie Listenge- neratoren sind verboten. -}

--Voraussetzung: sumdv funktion benutzen um eine Liste (siehe Aufgabe) zu herstellen
--Ergebnis: sumdv ist eine Funktion , welche eine liste von Int annimt und ein Int ist geliefert. (Deutsches Zustandspassiv)
sumdv :: [Int] -> Int --Funktionssignatur 
sumdv [] = 0 -- Definition (Basecase, sodass die Funktion nicht endlich läuft)
sumdv (x:xs) = if (mod x 3 == 0 || mod x 4 == 0) && mod x 6 /= 0 then x + sumdv xs else sumdv xs -- Definition 

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

--Voraussetzung: zensiert Funktion benutzen um beliebiege Characteren durch * zu ersetzen 
--Ergebnis: zensiert Funktion nimmt ein beliebieg gewähltes Charakter in einfach '' und kreiert eine Kopie
--wo eine Ersertzung mit * ist geliefert
zensiert :: Char -> String -> String 
zensiert c [] = [] --Basecase / Rekursionsanker
zensiert c (x:xs) = if x == c then '*' : zensiert c xs else x : zensiert c xs

{- Tests:
> zensiert 'f' "funny"
"*unny"
> zensiert 'a' "abracadabra"
"*br*c*d*br*"
> zensiert 'k' "kararoke"
"*araro*e"
-}


{- (c) Implementiere eine Funktion dropTake, die eine Liste xs und eine natürliche Zahl n bekommt 
und sowohl die ersten n als auch die letzten n Elemente behält und alle anderen Elemente wegschmeißt. 
Gibt es weniger höchstens 2n Elemente, werden alle Elemente behalten. 
Beispiele:
dropTake 3 [0,1,2,3,4,5,6,7,8,9] => [0,1,2,7,8,9] ->keeps first and last 3 characters 
dropTake 4 [0,1,2,3,4,5,6] => [0,1,2,3,4,5,6] -> does not double the repeated value, creates new list with all elements of imput list except elements that are not the first or last n of the list 
dropTake 0 [0,1,2,3,4,5,6] => [] --is not keeping anything of the list hence passes an empty list 
-}
--Voraussetzung: taking an Int and a list and returning a list 
--Ergebnis: The selected number is then the amount of elements being "removed" from the front and back of the list and a new list is returned.
dropTake :: Int -> [a] -> [a]
dropTake n xs = take n xs ++ drop (length xs - n) xs  -- Take first elements ++ drop all elements except last n elements

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
{- (a) Implementiere eine Funktion lowhigh, die eine Liste xs mit ganzen Zahlen erhält 
und zwei natürliche Zahlen liefert: Die eine Zahl gibt an, wie viele Elemente der Liste 
negativ waren und die zweite Zahl gibt an, wie viele Elemente der Liste positiv waren. 
Beispiel:
lowhigh [−2,3,0,−4,1,0,4] => (2,3) -}

--Voraussetzung: takes a list of Int and returns a tuple of two Ints 
--Ergebnis: evaluates the givven elements of a list and then the ammount of negatiev and positive values is retuned in a tupel.
lowhigh2 :: [Int] -> (Int, Int)
lowhigh2 xs = lowhighhelper xs 0 0 

lowhighhelper :: [Int] -> Int -> Int -> (Int,Int)
lowhighhelper [] n p = (n,p) 
lowhighhelper (x : xs) n p
  | x < 0 = lowhighhelper xs (n + 1) p
  | x > 0 = lowhighhelper xs n (p + 1)
  | otherwise = lowhighhelper xs n p

 {-Test:
 ghci> lowhigh2 [1,2,-3,0]
(1,2)
ghci> lowhigh2 [-100,-200,-9,0]
(3,0)
ghci> lowhigh2 [-2,3,0,-4,1,0,4]
(2,3)
-} 

--another way could also be:

lowhigh :: [Int] -> (Int, Int)
lowhigh xs = (length (filter (<0) xs), length (filter (>0) xs))

{- Tests:
ghci> lowhigh [-2,-1,1,2]
(2,2)
ghci> lowhigh [-1,0,-1]
(2,0)
ghci> lowhigh [1,10,-100]
(1,2)
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
--Voraussetzung: write two funtions in which one calls the other 
--Ergebnis: applies the function two times for the values >0 and <0 and returns the count of values complying it in a tuple (<0,>0)
lowhighFunc :: (Int -> Int) -> [Int] -> (Int, Int)
lowhighFunc f xs = (length (filter (<0) (map f xs)), length (filter (>0) (map f xs)))

f :: Int -> Int
f x = x^2 -5

{- Tests:
ghci> lowhighFunc f [-2,3,0,-4,1,0,4]
(4,3)
ghci> lowhighFunc f [1,2,3,0]
(3,1)
lowhighFunc f [-2,-1,-4,-5,-7,-200,0]
(3,4)
-}

{- (c) Implementiere eine Funktion absmin, welche eine Liste mit Gleitkommazahlen bekommt 
und den Betrag derjenigen Zahl liefert, die den kleinsten Abstand zur 0 hat. 
Verwende die map-Funktion.
absmin [1.6 , −0.1 , −7.0 ,2.2] => 0.1
 -}

--Voraussetzung: keine
--Ergebnis: it goes through the list and applies abs (absolut value) and returns the closest number to 0 (minnum).
absmin :: [Float] -> Float
absmin xs = minimum (map abs xs)

{- Tests:
ghci> absmin [1.6,-1.0,-7.0,2.2]
1.0
ghci> absmin [1.6,-0.1,-7.0,2.2]
0.1
ghci> absmin [1.9,-0.3,-3.9,0.2]
0.2
-}

{- (d) Implementiere ein Funktion toBools, die eine Liste xs mit Elementen beliebigen Typs sowie 
ein Pra ̈dikat p :: a -> Bool bekommt und all Elemente der Liste gema ̈ß des Pra ̈dikats p 
in True oder False umwandelt. Verwende die Funktion map.
toBools (<2) [ −4 ,2 ,3 ,0 ,1] => [True, False , False , True, True] -}

--Voraussetzung: keine
--Ergebnis: The program uses the function map to evaluate a list of elements fullfiling the condition x>3 and retuns a list of Bool.
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

{-heron :: Float -> (Float, Int)
heron a = (heronHelper a 1 0, heronHelper a 1 1)

heronHelper :: Float -> Float -> Int -> Float
heronHelper a x n
    | n == 10 = x
    | otherwise = heronHelper a ((x + a/x)/2) (n+1)-}

{- Tests:
> heron 2
(1.4142135,10)
> heron 3
(1.7320508,10)
> heron 4
(2.0,10)
-}