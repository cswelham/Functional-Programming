--Name: Connor Welham
--ID: 1508018

--Question 1
halve :: [a] -> ([a],[a])
halve [] = ([], [])
halve x = splitAt (((length x)) `div` 2) x

--Question 2
meld :: [Int] -> [Int] -> [Int]
meld [] xs = xs
meld xs [] = xs
meld (x:xs) (y:ys) = if x < y
                        then x:meld xs (y:ys)
                        else y:meld (x:xs) ys

--Question 3
integerSort :: [Int] -> [Int]
integerSort [] = []
integerSort [x] = [x]
integerSort (xs) = 
    meld (integerSort half1) (integerSort half2)
    where
        (half1, half2) = halve xs

--Question 4
replicate :: Int -> a -> [a]
replicate x y = concat[[y] | x <- [1..x]]

--Question 5
--Example of [f x | x <- xs, p x] where multiply3 is f and even is p
multiply3 x = x * 3
attempt1 xs = [multiply3 x | x <- xs, even x]
attempt2 xs = map (multiply3) . filter even $ xs
--Therefore [f x | x <- xs, p x] can be expressed as:
--map (f) . filter p $ xs

--Question 6
d2i :: [Int] -> Int
d2i xs = foldl (\x y -> 10*x+y) 0  xs

--Question 7
--i) TypeCheck- error: parse error on input `,'
--The inverted speech marks (`) suggest a is a function that takes two arguments, one before and one after. 
--However, here it is used in a list and has no arguments. This causes an error on the type-check.
--The list contains functions (or characters depending on the speech marks) and numbers which does not type check as they are not all the same type.

--ii) TypeCheck - [(*), 0, (+)] :: (Num a, Num (a -> a -> a)) => [a -> a -> a]
--Here is a list containing a function *, a number 0 and a function +. This does type check as you can not have functions in a list with numbers.

--iii) TypeCheck - error: parse error on input `2'
--This is a list of pairs with a number followed by a boolean value.
--However, the 2 has inverted speech marks suggesting it is a function when it is actually a number.
--This means the two pairs do not have the same types in the first argument and cause the error.

--iv) TypeCheck - No instance for (Foldable ((,,,) Integer Integer Integer)) arising from a use of `length'
--This is the function length followed by a list of 4 numbers. 
--However, the numbers are not in a list format of [] so length can not be apllied to them and fails the type check.

--Question 8
data Tree a = Node (Tree a) (Tree a) | Leaf a

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree (Node _ _)  (Leaf _)   = Node undefined undefined
zipTree (Leaf _)    (Node _ _) = Node undefined undefined
zipTree (Leaf a)     (Leaf b)     = Leaf (a,b)
zipTree (Node l1 r1) (Node l2 r2) = 
    let l = zipTree l1 l2
        r = zipTree r1 r2 
    in Node l r 

zipWidthTree f (Node _ _)  (Leaf _)   = Node undefined undefined
zipWidthTree f (Leaf _)    (Node _ _) = Node undefined undefined
zipWidthTree f (Leaf a)    (Leaf b)     = f Leaf (a,b)
zipWidthTree f (Node l1 r1) (Node l2 r2) = 
    let l = zipWidthTree f l1 l2
        r = zipWidthTree f r1 r2 
    in  Node l r 

--Question 9
--i)
type Var1 = String

data Expr = C Float | V Var1 | Expr :+ Expr | Expr :- Expr | Expr :*Expr | Expr :/ Expr | Let Var1 Expr Expr

--ii)
evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2)    = (evaluate e1) + (evaluate e2)
evaluate (e1 :- e2)    = (evaluate e1) - (evaluate e2)
evaluate (e1 :* e2)    = (evaluate e1) * (evaluate e2)
evaluate (e1 :/ e2)    = (evaluate e1) / (evaluate e2)
evaluate (Let x e1 e2) = (evaluate' e2 (evaluate e1)) 
evaluate (V x)         = error "Unbound Variable"

evaluate' :: Expr -> Float -> Float
evaluate' (e1 :+ e2) x    = (evaluate' e1 x) + (evaluate' e2 x)  
evaluate' (e1 :- e2) x    = (evaluate' e1 x) - (evaluate' e2 x)  
evaluate' (e1 :* e2) x    = (evaluate' e1 x) * (evaluate' e2 x) 
evaluate' (e1 :/ e2) x    = (evaluate' e1 x) / (evaluate' e2 x) 
evaluate' (V x) y         = y
evaluate' (C x) y         = x
evaluate' (Let x e1 e2) y = (evaluate' e2 (evaluate e1))