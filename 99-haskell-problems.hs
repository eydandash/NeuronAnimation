-- All Problems: https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

import Data.Char
import Data.List

-- 1 to 10: https://wiki.haskell.org/99_questions/1_to_10

-- Question 1
-- Find the last element of a list
myLast :: [a] -> a
myLast is = head reverse_list
  where reverse_list = reverse is

-- Question 2
-- Find second last element of a list
mySecondLast :: [a] -> a
mySecondLast is = head (drop (length is - 2) is)

-- Question 3
-- Find k'th element of a list
elementat :: [a] -> Int -> a
elementat list index = head (drop (index-1) list)

-- Question 4
-- Find number of elements in a list
myLength :: [a] -> Int
myLength list = length list

-- Question 5
-- Reverse a list
myReverse :: [a] -> [a]
myReverse list = reverse list

-- Question 6
-- Find out whether a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse (list)

-- Question 7
-- Flatten a nested list structure

-- Question 8
-- Eliminate consecutive duplicates of elements
compress :: Eq a => [a] -> [a]
compress list = [head (head x) | x <- group (group list)]

-- Question 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in seperate sublists.
-- pack :: Eq a => [a] -> [a]
-- pack list = [x | x <- group(list)]

-- Question 10
-- Run length encoding of a list.

-- Question 11
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.


-- Question 12
-- Decode a run length encoded list

-- Question 13
-- 

-- Question 14
-- Duplicate the elements of a list
dupli :: [Int] -> [Int]
dupli inputList = concat [[x,x] | x <- inputList]

