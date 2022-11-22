-- Informatik A WiSe 20/21
-- 4.12.2020
-- Max Willert

module Datentypen where

-- Zahldatentypen
-- ---------------

-- Die größtmögliche Int-Zahl (-> Zweierkomplement)
a :: Int
a = 2^63-1

-- Die kleinstmögliche Int-Zahl (-> Zweierkomplement)
b :: Int
b = -2^63

-- c sollte EIGENTLICH 2^63 sein (-> Überlauf)
c :: Int
c = a+1

-- Mit Integer kann man beliebig große Zahlen definieren
d :: Integer
d = 2^200

-- div und mod
e, f :: Int
e = 13`div`5
f = 13`mod`5
-- ` <- accent grave

-- Rundungsfehler bei Float und Double
g :: Double -- 64 Bit
g = (2.0*10^16+1)/(10^16)
h :: Float -- 32 Bit
h = (2.0*10^7+1)/(10^7)
