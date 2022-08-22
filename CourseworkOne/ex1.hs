--Name: Connor Welham
--ID: 1508018

twice f x = f (f x)
square n = n * n
naturals = [0..]
ones = 1 : ones

--Question 1
--a) twice :: (t -> t) -> t -> twice
--b) twice not False :: Bool
--c) square :: Num a => a -> a
--d) twice square :: Num t => t -> twice
--   twice uses type t so the Num a from the suare function is defined
--   as Num t
--e) twice square 5:: Num t => t
--f) head :: [a] -> a
--g) head naturals :: Integer
--h) 0

--Question 2
--(3 *) :: Num a => a -> a

--Question 3
--(* 3) = λa -> a * 3

--Question 4
--square (square 3)

--Question 5
--map (2 *) naturals

--Question 6
ord :: Char -> Int
ord = fromEnum
chr :: Int -> Char
chr = toEnum

capitalize :: Char -> Char
capitalize x = if (x >= 'a' && x <= 'z') then chr $ ord x + ord 'A' - ord 'a' else x

--Question 7
capitalize_string :: String -> String
capitalize_string y = map (capitalize) y

--Question 8
or_list :: [Bool] -> Bool 
or_list bs = foldr (||) False bs

--Question 9
--i)
flock :: Integer -> String
flock k = concat["sheep\n" | k <- [1..k]]

--ii)
flock2 :: Integer -> String
flock2 k = concat $ replicate (fromIntegral k) "sheep\n"

--iii)
a_row_of_sheep :: Integer -> String
a_row_of_sheep k = concat["sheep " | k <- [1..k]]

--iv)
--big_flock :: Integer -> String
--big_flock k = unlines $ map (unwords . flip replicate "sheep") [1..(fromIntegral k)]
big_flock :: Integer -> String
big_flock k = concat[(a_row_of_sheep k) ++ "\n" | k <- [1..k]]