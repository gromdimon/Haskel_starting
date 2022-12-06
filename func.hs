-- This are different methods of building functions
-- Functionality of funcs
{-
Voraussetzung:              y /= 0
Ergebnis:                   Teilen x durch y
Funktionssignatur:          teilen :: Float -> Float -> Float
Definintion:                teilen x y = x/y
-}


-- Pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"  -- Alternatively x = _

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x


-- Guards
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

-- Vor.: keine
-- Erg.: True ist genau dann geliefert, wenn xs das Element y enthält.
enthaelt :: Eq a => a -> [a] -> Bool
enthaelt y [] = False -- y ist nicht in der leeren Listen
enthaelt y (x:xs)
    | y == x = True -- 1. Fall y ist genau der Kopf der Liste
    | otherwise = enthaelt y xs -- 2. Fall y ist evtl. woanders
    

-- Let <definitions> in <expression>
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea


-- Case <expression> of pattern -> result
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."