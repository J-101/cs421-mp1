--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = [] --- If empty ret empty list, base case
mytake n (x:xs) --- Get each number
    | n <= 0 = []
    | otherwise = x : mytake (n-1) xs
--- Used the Basic Recursion slides, in particular mylength for reference

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n xs | n <= 0 = xs --- If n < 0, ret whole list
mydrop n (_:xs) = mydrop (n-1) xs --- Drops first n elements

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xs = revAux [] xs
  where revAux acc [] = acc --- Auxiliary function for reversing
        revAux acc (x:xs) = revAux (x:acc) xs
--- Accumulating recursion slide from notes used for reference
--- StackOverflow reference: https://stackoverflow.com/questions/33283905/recursive-functions-that-reverse-a-list-in-haskell

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] ys = ys --- If first list emp, ret second
app (x:xs) ys = x : app xs ys --- Append first list words to second list

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist xs = [x + 1 | x <- xs] --- increments each element by 1
--- Referenced for list comprehension info: https://wiki.haskell.org/List_comprehension

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0 --- If empty sum is 0
sumlist (x:xs) = x + sumlist xs --- Add head to current tail sum
--- Used StackOverflow implementation: https://stackoverflow.com/questions/51279298/haskell-sum-up-the-first-n-elements-of-a-list

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a, b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x, y) : myzip xs ys --- Pair head elements then recurse

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: Num a => [a] -> [a] -> [a]
addpairs xs ys = [x + y | (x, y) <- myzip xs ys] --- Sum up corresponding elements from lists

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones --- Infinite list of ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = enumFrom 0 --- Enumerates through numbers from 0
--- Didn't need to use recursion for this func

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = fibs
  where
    fibs = 0 : 1 : addpairs fibs (tail fibs) --- Fibonacci starts with 0 and 1 (base cases)
--- Referenced Tail Recursion slides in notes
--- Used this StackOverflow for reference: https://stackoverflow.com/questions/1105765/generating-fibonacci-numbers-in-haskell

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add x ys = insert  ys
  where
    insert  [] = [x] --- Where is an alternate syntax to defining beforehand
    insert  (y:ys)
      | x == y    = y : ys
      | x > y     = y : insert ys
      | otherwise = x : y : ys

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union xs ys = insert xs ys
  where
    insert [] ys = ys
    insert xs [] = xs
    insert (x:xs) (y:ys)
      | x < y     = x : insert xs (y:ys)
      | x > y     = y : insert (x:xs) ys
      | otherwise = x : insert xs ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect xs ys = insert xs ys
  where
    insert [] _ = []
    insert _ [] = []
    insert (x:xs) (y:ys)
      | x < y     = insert xs (y:ys)
      | x > y     = insert (x:xs) ys
      | otherwise = x : insert xs ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = let ps = powerset xs
                  in ps `union` [add x p | p <- ps]

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs  --- Apply func to head and recursively to tail
--- StackOverflow custom map function reference: https://stackoverflow.com/questions/19216243/writing-a-custom-map-function
--- Also used higher order functions slides for reference
inclist' :: Num a => [a] -> [a]
inclist' xs = myMap (+1) xs  --- Use myMap custom func to increment each element by 1

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
myFold :: (b -> a -> b) -> b -> [a] -> b
myFold _ acc [] = acc
myFold f acc (x:xs) = myFold f (f acc x) xs

sumlist' :: Num a => [a] -> a
sumlist' xs = myFold (+) 0 xs  --- Use myFold to sum elements of the list

{- References 
Textbook: https://learnyouahaskell.com/chapters
Recursion StackOverflow: https://stackoverflow.com/questions/14820138/understanding-recursion-in-haskell
Class Slides/Lectures
Function Explanations: https://www.haskell.org/tutorial/functions.html
Used ChatGPT to assist with error checking
More references provided along with some of the functions
-}