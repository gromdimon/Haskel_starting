-- Informatik A WiSe 20/21
-- 10.01.2021
-- Max Willert
module Akkumulator where

-- Akkumulatortechnik
----------------
-- Definitionen:
-- Eine Rekursion ist linear, wenn in jedem Aufruf höchstens ein rekursiver
-- Aufruf erfolgt. Ansonsten ist sie nicht-linear.
-- Ein rekursiver Aufruf heißt schlicht, wenn dieser direkt den Wert der
-- aufrufenden Funktion liefert.
-- Eine Funktion ist endrekursiv, wenn alle Funktionsaufrufe schlicht sind.

fak :: Integer -> Integer
fak 0 = 1
fak n = n * fak (n-1)

-- Die Funktion fak ist eine lineare Rekursion. fak (n-1) ist nicht schlicht,
-- da das Ergebnis von fak (n-1) nicht sofort das Ergebnis von fak n liefert,
-- es muss vorher noch eine Multiplikation mit n durchgeführt werden. Die
-- Funktion ist dementsprechend nicht endrekursiv. Die Anzahl der
-- Multiplikationen beim Aufruf von fak n beträgt genau n.

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Die Funktion fib ist nicht-linear und nicht endrekursiv. Sei T(n) die
-- Anzahl der Additionen, die beim Aufruf von fib n gemacht werden müssen.
-- Dann kann man T(n) einfach rekursiv definieren:

-- T(0) = T(1) = 0, denn beim Aufruf von fib 0 und fib 1 wird KEINE Addition
-- durchgeführt
-- T(n) = 1 + T(n-1) + T(n-2), wir berechnen alle Additionen für den Aufruf
-- fib (n-1), dann alle Additionen für den Aufruf fib (n-2),
-- hinzu kommt noch eine Addition

-- Man kann zeigen, dass T(n) ca. so groß ist wie 1.62^n. Das bedeutet, beim
-- Aufruf von fib n werden ca. 1.62^n viele Additionen durchgeführt. Zu viel!

-- Bisher: Sag mir mal das Ergebnis für das kleinere Problem, ich rechne dann weiter.
-- Mit Akkumulatortechnik: Berechne du das Ergebnis, das hier weiß ich schon.

-- Hier ist eine endrekursive Lösung mit Akkumulatortechnik: Um eine Fibonacci-
-- Zahl zu berechnen, müssen wir immer nur deren 2 Vorgänger kennen. Diese
-- werden wir bei den rekursiven Aufrufen mitschleppen und entsprechend
-- aktualisieren.

fastFib :: Integer -> Integer
fastFib n = help 0 1 n where
   help akk1 akk2 0 = akk1
   help akk1 akk2 n = help akk2 (akk1+akk2) (n-1)

-- Beim Aufruf von fastFib werden nur noch n Additionen benötigt.

-- Die folgende Funktion löst das Fakultätsprogramm und ist endrekursiv.
-- Es werden n Multiplikationen benötigt.

fastFak :: Integer -> Integer
fastFak n = help 1 n where
   help akk 0 = akk
   help akk n = help (n*akk) (n-1)