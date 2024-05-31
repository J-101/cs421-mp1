--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

--- StackOverflow for reference on Cons: https://stackoverflow.com/questions/41558870/what-does-cons-and-mean-in-haskell

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons xs = insert (reverse xs) Nil
  where
    insert []     acc = acc
    insert (x:xs) acc = insert xs (Cons x acc) --- Converts head and then does the tail

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list list = insert list []
  where
    insert Nil acc         = acc
    insert (Cons x xs) acc = insert xs (acc `append` x)
    append [] y        = [y] --- Prepends element to list then continues with the rest of list
    append (y:ys) z    = y : append ys z --- Appends rest of list

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp n)   = n
eval (PlusExp []) = 0
eval (MultExp []) = 1
eval (PlusExp es) = sum (map eval es) --- Evaluate + sum all sub-expressions from PlusExp
eval (MultExp es) = product (map eval es) --- Evaluate + multiply all sub-expressions from MultExp

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' [] = Nil
list2cons' (x:xs) = Cons x (list2cons xs) --- Creates list where first element is x, rest of list made recursively converting tail

--- ### BinTree

data BinTree a = Node a (BinTree a) (BinTree a)
               | Leaf
  deriving (Show)

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node x left right) = x + sumTree left + sumTree right --- Sums up all elements in a binary tree

--- ### SimpVal

data SimpVal = IntVal Integer
             | BoolVal Bool
             | StrVal String
             | ExnVal String
  deriving (Show)

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal x) (IntVal y) = IntVal (x `op` y)
liftIntOp _ _ _ = ExnVal "not an IntVal!"
--- Operator StackOverflow Reference: https://stackoverflow.com/questions/40033307/operator-as-an-argument-in-haskell

{- References 
Textbook: https://learnyouahaskell.com/chapters
Class Slides/Lectures (mainly week 2)
Function Explanations: https://www.haskell.org/tutorial/functions.html
Used ChatGPT to assist with error checking
More references provided along with some of the functions
-}