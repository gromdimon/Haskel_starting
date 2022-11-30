-- FUNKTIONEN IN HASKELL
module Aeins where
import Data.Char



-- A
{- Im ersten Video von Einheit 6 wurden die unterschiedlichen Arten von Ausdru ̈cken in Haskell genannt.
Gib einen Ausdruck an, indem alle fu ̈nf Arten auftauchen, d.h. er soll Konstanten, arithmetische,
boolesche und bedingte Ausdru ̈cke sowie Funktionsaufrufe beinhalten. Der Typ des Ausdrucks soll String sein
und Haskell muss in der Lage sein, den Wert des Ausdrucks zu berechnen.
Welchen Wert hat euer Ausdruck und wie kommt dieser zustande? Begru ̈nde außerdem, warum dein Ausdruck alle
genannten Forderungen erfu ̈llt. -}

ausdruck = case (4 `mod` 2) of 1 -> True || 4`div`2 == 2; 2 -> reverse "Zwei" == "iewZ"; 3 -> 2+4 == 6; otherwise -> False
-- Dieses Ausdrueck hat Konstanten (True), Arithmetische ausdruecke (x + 0), Boolesche ausdruecke (&&),
-- Bedingte ausdruecke (case of), Funktionsaufrufe (reverse)

{- Test(s):
> ghci
> :l a1.hs
> show ausdruck
"False" -}



-- B
{- Implementiere eine Funktion calc, welche zwei ganze Zahlen a und b sowie einen String op bekommen.
Der Eingabestring op soll  ̈plus ̈,  ̈minus ̈,  ̈mal ̈ oder  ̈hoch ̈ sein und die jeweils beschriebene Operation
auf die beiden Eingabezahlen anwenden. Wenn bei  ̈hoch ̈ der zweite Operand negativ ist, darf ein Fehler geliefert werden.
(Beispiel: calc 2  ̈hoch ̈ 3 = 8) -}

-- Voraussetzung: a und b sind ganzen Zahlen
-- Ergebnis: Beschriebende von Eingabestring Operation auf die beiden Eingabezahlen
calc :: Int -> String -> Int -> Int   -- Signatur
calc a "plus" b = a + b     -- Make sum for a and b
calc a "minus" b = a - b    -- Make substraction for a and b
calc a "mal" b = a * b      -- Make multiplication for a and b
calc a "hoch" b =           -- Make potent. for a and b. If b is negative, then catch exception
        if (b>=0) then (a ^ b)
        else error"Bitte gib nicht negative Zahl!"
calc _ _ _ = error"Kuatsch!"             -- Otherwise 0 for some error

{- Tests:
> calc 4 "plus" (-3)
1
> calc (-4) "minus" 7
-11
> calc (-11) "mal" 0
0
> calc (-3) "hoch" 5
-243
> calc (-3) "hoch" (-5)
*** Exception: Bitte gib nicht negative Zahl!
CallStack (from HasCallStack):
  error, called at 06/a1.hs:37:14 in main:Aeins
-}



-- C
{- Implementiere eine Funktion spanne, die vier Eingabezahlen bekommt und an- schließend den Abstand zwischen
der gro ̈ßten und der kleinsten der vier Eingabe- zahlen liefert.
(Beispiel: spanne (-2) 0 42 3 = 44) -}

-- Voraussetzung: vier Eingabezahlen in Int
-- Ergebnis: Abstand zwischen der groeßten und der kleinsten der vier Eingabezahlen
spanne :: Int -> Int -> Int -> Int -> Int   -- Signatur
spanne a b c d = (max - min)  -- Returning difference between 2 numbers
    where list = a:b:c:d:[]; max = maximum list; min = minimum list   -- Defining maximum and minimum of numbers

{- Tests:
> spanne 8 5 4 6
4
> spanne (-2) 0 43 3
45
> spanne 0 0 0 0
0
-}



-- D
{- Implementiere eine Funktion spiegeln, die einen Großbuchstaben (also einen Char) bekommt und denjenigen
Großbuchstaben liefert, der entsteht, wenn das Alphabet in der Mitte gespiegelt wird. Hier helfen die
Funktionen ord und chr, vergiss also nicht, das Modul Data.Char zu importieren.
(Beispiele: ’A’->’Z’, ’B’->’Y’, ’F’->’U’, ’Z’->’A’, ’Y’->’B’, ’U’->’F’, etc.) -}

-- Voraussetzung: Großbuchstaben
-- Ergebins: Großbuchstaben entsprechend der Aufgabe
spiegeln :: Char -> Char
spiegeln a =
    let ind = ord a   -- Ind entschpricht nummer in ASCII Tabelle von char
        shift = abs ( abs ind - 77)   --Shift bedeutet ein Zahl fuer neue Buchstabe
    in if ind <= 77 && ind >= 65 then chr (78 + shift) else if ind > 77 && ind <= 90 then chr (78 - shift) else error"Falsche Eingabe"

{- Tests:
> spiegeln 'A'
'Z'
> spiegeln 'C'
'X'
> spiegeln 'M'
'N'
> spiegeln '<'
*** Exception: Falsche Eingabe
CallStack (from HasCallStack):
  error, called at 06/a1.hs:86:113 in main:Aeins
-}