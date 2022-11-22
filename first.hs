import Data.List
import System.IO

{-
That was first line in haskell
main = putStrLn ("Hello, World!")
-}

maxInt = maxBound :: Int -- Integers from -2^63 to 2^63
bigFloat = 3.12345678901 --Double or Float
-- Bool
-- Char '
-- Tuple

always5 :: Int
always5 = 5
modEx = mod 5 4
negNumEx = 5 + (-4)
num9 = 9 :: Int
sqrtof9 = sqrt (fromIntegral num9)

-- Built in math Functions
piVal = pi
ePow9 = exp 9
logof9 = log 9
truncateVal = truncate 9.9999
roundVal = round 9.9999
ceilingVal = ceiling 9.9999
floorVal = floor 9.9999

trueandfalse = True && False
trueorfalse = True || False
nottrue = not(True)