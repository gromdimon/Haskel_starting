-- Informatik A WiSe 20/21
-- 10.01.2021
-- Max Willert
module Evaluation where

-- Ein Haskellprogramm ist die Auswertung eines Ausdrucks
-- zu einem Wert. Mindestens folgende Fragen ergeben sich:
-- Wie wird ausgewertet?
-- Was wird (nicht) ausgewertet?
-- In welcher Reihenfolge wird ausgewertet?

add :: Int -> Int -> Int
add x y = x + y

quad :: Int -> Int
quad n = n * n

-- Call-by-value: (nicht in Haskell!)
-- Regeln: Werte zuerst die inneren Funktionen aus und dann die äußeren.
-- Sollte es mehrere innere Funktionen geben, so werden in der Regel erst
-- die Funktionen, die am weitesten links stehen, ausgeweitet.
-- Beispiel:
-- add (quad 3) (quad 7) = add (3*3) (quad 7) = add 9 (quad 7)
-- = add 9 (7*7) = add 9 49 = 9 + 49 = 58

-- Call-by-name / Lazy Evaluation: (so arbeitet Haskell!)
-- Grundidee: Werte Ausdrücke nur dann aus, wenn es zwingend erforderlich
-- ist.
-- Regeln: Werte zuerst die äußeren Funktionen aus, sofern dies möglich
-- ist, und dann die inneren. Sollte es mehrere innere Funktionen geben,
-- so werden in der Regel erst die Funktionen, die am weitesten links
-- stehen, ausgeweitet.
-- Beispiele:
-- quad (add 3 7) = (add 3 7)*(add 3 7) = (3+7)*(add 3 7) = 10*(add 3 7)
-- = 10*(3+7) = 10*10 = 100

-- Fakt: Sollte die Auswertung eines Ausdrucks bei beiden Varianten
-- terminieren, so ist das Ergebnis in beiden Fällen dasselbe.

inf :: Int
inf = 1 + inf

-- fst :: (a,b) -> a
-- fst (x,y) = x

-- fst (0,inf) kann mit Call-by-value nicht ausgewertet werden, mit
-- call-by-name wird es zu 0 ausgewertet

-- [k..n]
   -- | k > n = []
   -- | otherwise = k : [k+1..n]
-- take :: Int -> [a] -> [a]
-- take 0 xs = []
-- take n [] = []
-- take n (x:xs) x : take (n-1) xs

-- Auswertung von take 2 [3..7] mithilfe von Lazy Evaluation:
-- take 2 [3..7]
-- = take 2 (3 : [3+1..7])
-- = 3 : (take (2-1) [3+1..7])
-- = 3 : (take 1 [3+1..7])
-- = 3 : (take 1 [4..7])
-- = 3 : (take 1 (4 : [4+1..7]))
-- = 3 : 4: (take (1-1) [4+1..7])
-- = 3 : 4: (take 0 [4+1..7])
-- = 3 : 4 : []
-- Es ist vollkommen egal, wie lange die Eingabeliste ist, es wird lediglich
-- auf die ersten beiden Elemente zugegriffen. Damit werden unendliche Listen
-- prinzipiell möglich:

-- [k..] = k : [k+1..]

-- Die Auswertungsreihenfolge von take 2 [3..] ist dieselbe wie im obigen
-- Beispiel.

-- Vorteile von Call-by-name im Vergleich zu Call-by-value:
-- 1. Lazy Evaluation ermöglicht prinzipiell unendliche Strukturen.
-- 2. Es gibt mehr Ausdrücke, die durch Lazy Evaluation ausgewertet werden
   -- können.
-- 3. Die Laufzeit kann drastisch verringert werden.

-- Selbsttest: Was ist Signatur und Spezifikation der folgenden Funktion (!!) ?
-- []!!n = error"Gibt es nicht"
-- (x:xs)!!0 = x
-- (x:xs)!!n = xs!!(n-1)