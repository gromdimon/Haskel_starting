import Data.List
import System.IO

{-
That was first line in haskell
main = putStrLn ("Hello, World!")
-}


-- Bool
-- Char '
-- Tuple
maxInt = maxBound :: Int -- Integers from -2^63 to 2^63
bigFloat = 3.12345678901 --Double or Float
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


{-Boolean-}
trueandfalse = True && False
trueorfalse = True || False
nottrue = not(True)


{-Lists-}
prime_num = [3,5,7,11]
more_prime = prime_num ++ [13,17,19]
even_more = 2 : more_prime
fav_nums = 2 : 7 : 21 : 66 :[]
multilist = [[1,2,3],[4,5,6]]
len_list = length prime_num
rev_list = reverse prime_num
is_empty_list = null prime_num
second_elem = prime_num !! 1
first_elem = head prime_num
last_elem = last prime_num
init_list = init prime_num
first_3_primes = take 3 prime_num
remove_list = drop 3 prime_num
is_elem_in_list = 7 `elem` prime_num -- I am not sure that ´ ` are correct
max_elem = maximum prime_num
min_elem = minimum prime_num
zero_ten = [0..10]
even_list = [2,4..20]
letter_list = ['A', 'C'..'Z']
ten_2 = take 10 (repeat 2)
ten_3 = replicate 10 3
list_time_2 = [x * 2 | x <- [1..10], x * 3 <= 50] -- 2, 4, 6, 8... if x*3 is less then 50
divis_by_4_or_5 = [x | x <- [1..100], x `mod` 4 == 0, x `mod` 5 == 0]
enorm_cmprhnsn = [[x*y | y <- [1..10]] | x <- [1..10]]
sorted_list = sort prime_num
sum_of_lists = zipWith (+) [1,2,3,4] [5,6,7,8]
filter_list = filter (>5) prime_num
evens_upto_20 = takeWhile (<=20)[2,4..]


{- Tuples -}
rand_tuple = (1, "random")
num = fst rand_tuple
str = snd rand_tuple
names = ["Bob", "Max", "Lena"]
nums = [1, 56, 789]
tuple_zip = zip names nums


{-Functions-}
get_tripple x = x*3 -- in ghci write "let get_tripple x = x*3"

{-main do
    putStrLn "What's your age?"
    age <- getLine
    putStrLn ("You are" ++ age ++ "y.o.")-}

addMe :: Int -> Int -> Int
-- funcname param1 param2 = operations (returned val)
addMe x y = x * y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (z, q) = (x - y, z + q)

wat_int :: Int -> Float
wat_int 3 = 0.001
wat_int 2 = 0.01
wat_int 1 = 0.1
wat_int x = 0 -- or wat_int _ = 0

