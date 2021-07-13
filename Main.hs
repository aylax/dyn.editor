import Data.List -- load all
import Data.List (nub, sort) -- load two
import Data.List hiding (nub, sort) -- load all expect two
import qualified Data.Map as M
import qualified Geometry as G



bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"


max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"


beep :: (Num a) => a -> a -> a -> a -> String
beep a b c d
    | otherwise = "ne"


map' :: (a -> a) -> [a] -> [a]  
map' _ [] = []  
map' f (x:xs) = f x : map f xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x


describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list." 

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs



quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    let ls = quicksort' [ y | y <- xs, y < x ]
        rs = quicksort' [ y | y <- xs, y >= x ]
        in  ls ++ [x] ++ rs




zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ [] _ = []
zipwith' _ _ [] = []
zipwith' f (x:xs) (y:ys) = f x y : zipwith' f xs ys




chain' :: (Integral a) => a -> [a]
chain' 1 = [1]
chain' n
    | even n = n : chain' (n `div` 2)
    | odd n = n : chain' (n * 3 + 1)


maximum'' :: (Ord a) => [a] -> a  
maximum'' = foldr1 (\x acc -> if x > acc then x else acc)  

reverse'' :: [a] -> [a]  
reverse'' = foldl (\acc x -> x : acc) []  
  
product'' :: (Num a) => [a] -> a  
product'' = foldr1 (*)  
  
filter'' :: (a -> Bool) -> [a] -> [a]  
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head'' :: [a] -> a  
head'' = foldr1 (\x _ -> x)  
  
last'' :: [a] -> a  
last'' = foldl1 (\_ x -> x)


-- ($) :: (a -> b) -> a -> b  
-- f $ x = f x

--  (.) :: (b -> c) -> (a -> b) -> a -> c  
-- f . g = \x -> f (g x)


