{-

    ANSWERS:
    https://github.com/anr/prog-haskell
    https://github.com/carlosgaldino/programming-in-haskell

-}

-- imports
import Data.Char
import Data.List

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 1 ----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
-- Give another possible calculation for the result of double (double 2).
quadruple :: Int -> Int
quadruple x = 4 * x

-- Question 2
-- Show that sum[x] = x for any number x.

-- Question 3
-- Define a function product that produces the product of a list of numbers, and show using your definition that product[2,3,4] = 24.
prodOfList :: [Int] -> Int
prodOfList [] = 1
prodOfList (n:ns) = n * prodOfList ns

-- Question 4
-- How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?
{-
    Before:

    qsort :: Ord a => [a] -> [a]
    qsort[] = []
    qsort(x:xs) = qsort smaller ++ [x] ++ qsort larger
                where 
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]
-}
qsort :: Ord a => [a] -> [a]
qsort[] = []
qsort(x:xs) = qsort smaller ++ [x] ++ qsort larger
            where 
                smaller = [a | a <- xs, a > x]
                larger = [b | b <- xs, b <= x]

-- Question 5
-- What would be the effect of replacing <= by < in the definition of qsort?
-- Hint: Conside the example qsort[2,2,3,1,1]

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 2 ----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
-- Parenthesise the following arithmetic expressions:
-- 2 ^ 3 * 4
-- 2 * 3 + 4 * 5
-- 2 + 3 * 4 ^ 5

-- Question 2
-- Work through the examples in the chapter:
-- head [1,2,3,4,5]
-- tail [1,2,3,4,5]
-- [1,2,3,4,5] !! 2
-- take 3 [1,2,3,4,5]
-- drop 3 [1,2,3,4,5]
-- length [1,2,3,4,5]
-- sum [1,2,3,4,5]
-- product [1,2,3,4,5]
-- [1,2,3] ++ [4,5]
-- reverse [1,2,3,4,5]
-- 1 `div` 0
-- head []
-- double x = x + x
-- quadruple x = double (double x)
-- quadruple 10
-- take (double 2) [1,2,3,4,5]
-- factorial n = product[1..n]
-- average ns = sum ns `div` length ns
-- factorial 10
-- average [1,2,3,4,5]

-- Question 3
-- The script below contains three syntactic errors. Correct these error and then check your script works properly:
-- n :: [Int] -> Int
-- n = a div (length xs)
--      where
--          a = 10
--          xs = [1,2,3,4,5]

-- Question 4
-- Show how the library function last that selects the last element of a nonempty list could be defined in terms of the library functions introduced in this chapter. Can you think of another possible definition?
myLast :: [Int] -> Int
myLast ns = ns !! ((length ns) - 1)

-- Question 5
-- Show how the library function init that removes the last element from a non-empty list could similarly be defined in two different ways.
myInit :: [Int] -> [Int]
-- myInit ns = take ((length ns) - 1) ns
myInit ns = reverse (drop 1 (reverse ns))

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 3 ----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
-- What are the types of the following values?
-- [’a’, ’b’, ’c’]
-- (’a’, ’b’, ’c’)
-- [(False, ’O’), (True, ’1’)]
-- ([False,True ], [’0’, ’1’])
-- [tail , init, reverse ]

-- Question 2
-- What are the types of the following functions?
-- second xs = head (tail xs)
-- swap (x , y) = (y, x)
-- pair x y = (x , y)
-- double x = x ∗ 2
-- palindrome xs = reverse xs == xs
-- twice f x = f (f x)
-- Hint: take care to include the necessary class constraints if the functions are defined using overloaded operators.
second :: Ord a => [a] -> a
second xs = head (tail xs)

swap :: (Int, Int) -> (Int, Int)
swap (x,y) = (y,x)

pair :: a -> a -> (a, a)
pair x y = (x, y)

double :: Int -> Int
double x = x * 2

palindrome :: Ord a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Question 3
-- Check your answers to the preceding two questions using GHCI.

-- Question 4
-- Why is it not feasible in general for function types to be instances of the Eq class? When is it feasible? Hint: two functions of the same type are equal if they always return equal results for equal arguments.

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 4 ----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
-- Using library functions, define a function halve :: [a] → ([a], [a]) that splits an even-lengthed list into two halves. For example:
-- > halve [1, 2, 3, 4, 5, 6]
-- ([1, 2, 3], [4, 5, 6])

halve :: [Int] -> ([Int], [Int])
halve ns = pair (take (div (length ns) 2) ns) (drop (div (length ns) 2) ns)

-- Question 2
-- Consider a function safetail :: [a] → [a] that behaves as the library function tail , except that safetail maps the empty list to itself, whereas tail produces an error in this case. Define safetail using:

-- (a) a conditional expression;
-- (b) guarded equations;
-- (c) pattern matching.

-- Hint: make use of the library function null .

safetail :: [a] -> [a]
safetail es | null es = es
            | otherwise = tail es

-- Question 3
-- In a similar way to ∧, show how the logical disjunction operator ∨ can be defined in four different ways using pattern matching.

-- Question 4
{-
    Redefine the following version of the conjunction operator using conditional
    expressions rather than pattern matching:
    True ∧ True = True
    _∧_ = False

-}

myAnd :: Bool -> Bool -> Bool
myAnd x y | (x==True) && (y==True) = True
          | otherwise = False
    
-- Question 5
{-
    Do the same for the following version, and note the difference in the number of conditional expressions required:
    True ∧ b = b
    False ∧ _ = False
-}

myAndTwo :: Bool -> a -> Bool
myAndTwo b a | (b==True) = True
             | (b==False) = False
             | otherwise = False

-- Question 6
{-
    Show how the curried function definition mult x y z = x ∗ y ∗ z can be understood in terms of lambda expressions.
    
    My Answer:
    mult x y z = (\x -> (\y -> (\z -> x * y * z)))
-}

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 5 ----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
-- Using a list comprehension, give an expression that calculates the sum 1^2 + 2^2 + . . . 100^2 of the first one hundred integer squares.

squaresToOneHundred :: Int -> Int
squaresToOneHundred n = sum[x^2 | x <- [1..100]]

-- Question 2
{-
    In a similar way to the function length, show how the library function replicate :: Int → a → [a] that produces a list of identical elements can be defined using a list comprehension. For example:

    > replicate 3 True
    [True,True,True ]

-}

myReplicate :: Int -> a -> [a]
myReplicate n a = [a | _ <- [1..n]]

-- Question 3
{-
    A triple (x, y, z) of positive integers is pythagorean if x^2 + y^2 = z^2. Using a list comprehension, define a function pyths :: Int → [(Int, Int, Int)] that returns the list of all pythagorean triples whose components are at most a given limit. For example:

    > pyths 10
    [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

-}



-- Question 4
{-

    A positive integer is perfect if it equals the sum of its factors, excluding the number itself. Using a list comprehension and the function factors, define a function perfects :: Int → [Int ] that returns the list of all perfect numbers up to a given limit. For example:
    
    > perfects 500
    [6, 28, 496]

-}

-- Question 5
{-

    Show how the single comprehension [(x , y) | x ← [1, 2, 3], y ← [4, 5, 6]] with two generators can be re-expressed using two comprehensions with single generators. Hint: make use of the library function concat and nest one comprehension within the other.

-}

-- Question 6
{-

    Redefine the function positions using the function find.

-}

-- Question 7
{-

    The scalar product of two lists of integers xs and ys of length n is given by the sum of the products of corresponding integers:
    
    n-1
     Σ   (xsi * ysi)
    i=0

    In a similar manner to the function chisqr, show how a list comprehension can be used to define a function scalarproduct :: [Int ] → [Int ] → Int that returns the scalar product of two lists. For example:
    
    > scalarproduct [1, 2, 3] [4, 5, 6]
    32  

-}

-- Question 8
{-

    Create the Caesar to code any word by n spaces. For example:
    > ceasar 2 "hello"
    "jgnnq"
    
    Modify the Caesar cipher program to also handle upper-case letters.

-}

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 6 ----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
{-

    Define the exponentiation operator ↑ for non-negative integers using the same pattern of recursion as the multiplication operator ∗, and show how 2 ↑ 3 is evaluated using your definition.

-}

-- Question 2
{-

    Using the definitions given in this chapter, show how length [1, 2, 3], drop 3 [1, 2, 3, 4, 5], and init [1, 2, 3] are evaluated.

-}

-- Question 3
{-

    Without looking at the definitions from the standard prelude, define the following library functions using recursion.

    – Decide if all logical values in a list are True:
    and :: [Bool] → Bool
    
    – Concatenate a list of lists:
    concat :: [[a]] → [a]
    
    – Produce a list with n identical elements:
    replicate :: Int → a → [a]
    
    – Select the nth element of a list:
    (!!) :: [a] → Int → a
    
    – Decide if a value is an element of a list:
    elem :: Eq a ⇒ a → [a] → Bool

    Note: most of these functions are in fact defined in the prelude using other library functions, rather than using explicit recursion.

-}

-- Question 4
{-

    Define a recursive function merge :: Ord a ⇒ [a] → [a] → [a] that merges two sorted lists to give a single sorted list. For example:

    > merge [2, 5, 6] [1, 3, 4]
    [1, 2, 3, 4, 5, 6]

    Note: your definition should not use other functions on sorted lists such as insert or isort, but should be defined using explicit recursion.

-}

-- Question 5
{-

    Using merge, define a recursive function msort :: Ord a ⇒ [a] → [a] that implements merge sort, inwhich the empty list and singleton lists are already sorted, and any other list is sorted by merging together the two lists that result from sorting the two halves of the list separately.

    Hint: first define a function halve :: [a] → [([a], [a])] that splits a list into two halves whose lengths differ by at most one.

-}

-- Question 6
{-

    Using the five-step process, define the library functions that calculate the sum of a list of numbers, take a given number of elements from the start of a list, and select the last element of a non-empty list.

-}

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 7 ----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
{-

    Show how the list comprehension [f x | x ← xs,p x] can be re-expressed using the higher-order functions map and filter .

-}

-- Question 2
{-

    Without looking at the definitions from the standard prelude, define the higher-order functions all , any, takeWhile, and dropWhile.

-}

-- Question 3
{-

    Redefine the functions map f and filter p using foldr.

-}

-- Question 4
{-

    Using foldl, define a functiondec2int :: [Int ] → Int that converts a decimal number into an integer. For example:
    
    > dec2int [2, 3, 4, 5]
    2345

-}

-- Question 5
{-

    Explain why the following definition is invalid:
    
    sumsqreven = compose [sum, map (↑2), filter even ]

-}

-- Question 6
{-

    Without looking at the standard prelude, define the higher-order library function curry that converts a function on pairs into a curried function, and, conversely, the function uncurry that converts a curried function with two arguments into a function on pairs.
    
    Hint: first write down the types of the two functions.

-}

-- Question 7
{-

    A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be defined as follows:
    
    unfold p h t x | p x = [ ]
    | otherwise = h x : unfold p h t (t x)

    That is, the function unfold p h t produces the empty list if the predicate p is true of the argument, and otherwise produces a non-empty list by applying the function h to give the head, and the function t to generate another argument that is recursively processed in the same way to produce the tail of the list. For example, the function int2bin can be rewritten more compactly using unfold as follows:
    
    int2bin = unfold (== 0) (‘mod‘2) (‘div‘2)
    
    Redefine the functions chop8 , map f and iterate f using unfold.

-}

-- Question 8
{-

    Modify the string transmitter program to detect simple transmission errors using parity bits. That is, each eight-bit binary number produced during encoding is extended with a parity bit, set to one if the number contains an odd number of ones, and to zero otherwise. In turn, each resulting ninebit binary number consumed during decoding is checked to ensure that its parity bit is correct, with the parity bit being discarded if this is the case, and a parity error reported otherwise.
        
    Hint: the library function error :: String → a terminates evaluation and displays the given string as an error message.

-}

-- Question 9
{-

    Test your new string transmitter program from the previous exercise using a faulty communication channel that forgets the first bit, which can be modelled using the tail function on lists of bits.

-}

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 8 ----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
{-

    The library file also defines a parser int :: Parser Int for an integer.Without looking at this definition, define int. Hint: an integer is either a minus symbol followed by a natural number, or a natural number.

-}

-- Question 2
{-

    Define a parser comment :: Parser () for ordinary Haskell comments that begin with the symbol -- and extend to the end of the current line, which is represented by the control character ’\n’.

-}

-- Question 3
{-

    Using our second grammar for arithmetic expressions, drawthe two possible parse trees for the expression 2 + 3 + 4.

-}

-- Question 4
{-

    Using our third grammar for arithmetic expressions, draw the parse trees for the expressions 2 + 3, 2 ∗ 3 ∗ 4 and (2 + 3) + 4.

-}

-- Question 5
{-

    Explain why the final simplification of the grammar for arithmetic expressions has a dramatic effect on the efficiency of the resulting parser. 
    
    Hint: begin by considering how an expression comprising a single number would be parsed if this step had not been made.

-}

-- Question 6
{-

    Extend the parser for arithmetic expressions to support subtraction and division, based upon the following extensions to the grammar:
    
    expr ::= term (+ expr | − expr | ε)
    term ::= factor (∗ term | / term | ε)

-}

-- Question 7
{-

    Further extend the grammar and parser for arithmetic expressions to support exponentiation, which is assumed to associate to the right and have higher priority than multiplication and division, but lower priority than parentheses and numbers. For example, 2 ↑ 3 ∗ 4 means (2 ↑ 3) ∗ 4. Hint: the new level of priority requires a new rule in the grammar.

-}

-- Question 8
{-

    Consider expressions built up from natural numbers using a subtraction operator that is assumed to associate to the left.

    (a) Define a natural grammar for such expressions.
    (b) Translate this grammar into a parser expr :: Parser Int.
    (c) What is the problem with this parser?
    (d) Show how it can be fixed. 
    
    Hint: rewrite the parser using the repetition primitive many and the library function foldl .

-}

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 9 ----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
{-

    Define an action readLine :: IO String that behaves in the same way as getLine, except that it also permits the delete key to be used to remove characters. Hint: the delete character is ’\DEL’, and the control string for moving the cursor back one character is "\ESC[1D".

-}

-- Question 2
{-

    Modify the calculator program to indicate the approximate position of an error rather than just sounding a beep, by using the fact that the parser returns the unconsumed part of the input string.

-}

-- Question 3
{-

    On some systems the game of life may flicker, due to the entire screen being cleared each generation. Modify the game to avoid such flicker by only redisplaying positions whose status changes.

-}

-- Question 4
{-

    Produce an editor that allows the user to interactively create and modify the content of the board in the game of life.

-}

-- Question 5
{-

    Produce graphical versions of the calculator and game of life programs, using one of the graphics libraries available from www.haskell .org.

-}

-- Question 6
{-

    Nim is a game that is played on a board comprising five numbered rows of stars, which is initially set up as follows:

    1 : ∗ ∗ ∗ ∗ ∗
    2 : ∗ ∗ ∗∗
    3 : ∗ ∗ ∗
    4 : ∗∗
    5 : ∗

    Two players take it in turn to remove one or more stars from the end of a single row. The winner is the player who removes the last star or stars from the board. Implement the game of nim in Haskell. Hint: represent the board as a list comprising the number of stars remaining on each row, with the initial board being [5, 4, 3, 2, 1].

-}

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 10 ---------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
{-

    Using recursion and the function add, define a multiplication function mult :: Nat → Nat → Nat for natural numbers.

-}

-- Question 2
{-

    Although not included in appendix A, the standard library defines data 
        Ordering = LT | EQ | GT 
    together with a function 
        compare :: Ord a ⇒ a → a → Ordering
    that decides if one value in an ordered type is less than (LT), equal to (EQ), or greater than (GT) another such value. Using this function, redefine the function occurs :: Int → Tree → Bool for search trees. Why is this new
    definition more efficient than the original version?

-}

-- Question 3
{-

    Consider the following type of binary trees:

    type Tree = Leaf Int | Node Tree Tree

    Let us say that such a tree is balanced if the number of leaves in the left and right subtree of every node differs by at most one, with leaves themselves being trivially balanced. Define a function balanced :: Tree → Bool that decides if a tree is balanced or not. Hint: first define a function that returns the number of leaves in a tree.

-}

-- Question 4
{-

    Define a function 
    
    balance :: [Int ] → Tree 
    
    that converts a non-empty list of integers into a balanced tree. Hint: first define a function that splits a list into two halves whose length differs by at most one.

-}

-- Question 5
{-

    Extend the tautology checker to support the use of logical disjunction (∨) and equivalence (⇔) in propositions.

-}

-- Question 6
{-

    Using the function isTaut together with the parsing and interaction libraries from the previous two chapters, define an interactive tautology checker that allows propositions to be entered from the keyboard in a user-friendly syntax. Hint: build a parser for propositions by modifying the parser for arithmetic expressions given in chapter 8.

-}

-- Question 7
{-

    Extend the abstract machine to support the use of multiplication.

-}

-- Question 8
{-

    Complete the following instance declarations:
    
    instance Monad Maybe where
    · · ·

    instance Monad [ ] where
    · · ·
    
    In this context, [ ] denotes the list type [a] without its parameter. Hint: first write down the types of return and >>= for each instance.

-}

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 11 ---------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
{-

    Redefine the combinatorial function choices using a list comprehension rather than the library functions concat and map.

-}

-- Question 2
{-

    Define a recursive function isChoice :: Eq a ⇒ [a] → [a] → Bool that decides if one list is chosen from another, without using the combinatorial functions perms and subs. Hint: start by defining a function that removes the first occurrence of a value from a list.

-}

-- Question 3
{-

    What effect would generalising the function split to also return pairs containing the empty list have on the behaviour of solutions?

-}

-- Question 4
{-

    Using choices, exprs, and eval, verify that there are 33, 665, 406 possible expressions over the numbers 1, 3, 7, 10, 25, 50, and that only 4, 672, 540 of these expressions evaluate successfully.

-}

-- Question 5
{-

    Similarly, verify that the number of expressions that evaluate successfully increases to 10, 839, 369 if the numeric domain is generalised to arbitrary integers. Hint: modify the definition of valid.

-}

-- Question 6
{-

    Modify the final program to:

    (a) allow the use of exponentiation in expressions;
    (b) produce the nearest solutions if no exact solution is possible;
    (c) order the solutions using a suitable measure of simplicity.

-}

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 12 ---------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
{-

    Identify the redexes in the following expressions, and determine whether each redex is innermost, outermost, neither, or both:

    1 + (2 ∗ 3)
    (1 + 2) ∗ (2 + 3)
    fst (1 + 2, 2 + 3)
    (λx → 1 + x) (2 ∗ 3)

-}

-- Question 2
{-

    Show why outermost evaluation is preferable to innermost for the purposes of evaluating the expression fst (1 + 2, 2 + 3).

-}

-- Question 3
{-

    Given the definition mult = λx → (λy → x ∗ y), show how the evaluation of mult 3 4 can be broken down into four separate steps.

-}

-- Question 4
{-

    Using a list comprehension, define an expression fibs :: [Integer ] that generates the infinite sequence of Fibonacci numbers 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, · · · using the following simple procedure:

    – the first two numbers are 0 and 1;
    – the next is the sum of the previous two;
    – return to the second step.

    Hint: make use of the library functions zip and tail. Note that numbers in the Fibonacci sequence quickly become large, hence the use of the type Integer of arbitrary-precision integers above.

-}

-- Question 5
{-

    Using fibs, define a function fib :: Int → Integer that returns the nth Fibonnaci number (counting from zero), and an expression that calculates the first Fibonacci number greater than one thousand.

-}

-- Question 6
{-

    Define appropriate versions of the library functions:

        repeat :: a → [a ]
        repeat x = xs where xs = x : xs
        take :: Int → [a ] → [a ]
        take 0 = [ ]
        take (n + 1) [ ] = [ ]
        take (n + 1) (x : xs) = x : take n xs
        replicate :: Int → a → [a ]
        replicate n = take n ◦ repeat

    for the following type of binary trees:

        data Tree a = Leaf | Node (Tree a) a (Tree a)

-}

-----------------------------------------------------------------------------------------------------------------------
-- Chapter 13 ---------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-- Question 1
{-

    Give an example of a function from the standard library in appendix A that is defined using overlapping patterns.

-}

-- Question 2
{-

    Show that add n (Succ m) = Succ (add n m), by induction on n.

-}

-- Question 3
{-

    Using this property, together with add n Zero = n, show that addition is commutative, add n m = add m n, by induction on n.

-}

-- Question 4
{-

    Using the following definition for the library function that decides if all elements of a list satisfy a predicate
    
        all p [ ] = True
        all p (x : xs) = p x ∧ all p xs

    complete the proof of the correctness of replicate by showing that it produces a list with identical elements, all (== x) (replicate n x ), by induction on n ≥ 0. 
    
    Hint: show that the property is always True.

-}

-- Question 5
{-

    Using the definition
        
        [ ] ++ ys = ys
        (x : xs) ++ ys = x : (xs ++ ys)

    verify the following two properties, by induction on xs:
    
        xs ++ [ ] = xs
        xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

    Hint: the proofs are similar to those for the add function.

-}

-- Question 6
{-

    The equation reverse (reverse xs) = xs can also be proved using a single auxiliary result, reverse (xs ++ [x ]) = x : reverse xs, which can itself be verified by induction on xs. Why might the proof using three auxiliary results as in this chapter be viewed as preferable?

-}

-- Question 7
{-

    Using the definitions

        map f [ ] = [ ]
        map f (x : xs) = f x : map f xs
        (f ◦ g) x = f (g x)
    
    show that 
    
        map f (map g xs) = map (f ◦ g) xs, 
        
    by induction on xs.

-}

-- Question 8
{-

    Using the definition for ++ given above, together with

    take 0 _ = [ ]
    take (n + 1) [ ] = [ ]
    take (n + 1) (x : xs) = x : take n xs
    drop 0 xs = xs
    drop (n + 1) [ ] = [ ]
    drop (n + 1) (_ : xs) = drop n xs

    show that take n xs ++ drop n xs = xs, by simultaneous induction on the integer n ≥ 0 and the list xs. Hint: there are three cases, one for each pattern of arguments in the definitions of take and drop.

-}

-- Question 9
{-

    Given the type declaration

    data Tree = Leaf Int | Node Tree Tree

    show that the number of leaves in a such a tree is always one greater than the number of nodes, by induction on trees. Hint: start by defining functions that count the number of leaves and nodes in a tree.

-}

-- Question 10
{-

    Given the equation comp' e c = comp e ++ c, show how to construct the recursive definition for comp', by induction on e.

-}