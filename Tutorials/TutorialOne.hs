module TutorialOne where

size :: Integer
size = 12 + 13

-- The function to square an integer.

square :: Integer -> Integer
square n = n * n

-- The function to double an integer.

double :: Integer -> Integer
double n = 2 * n

-- Question 1

squareOfDouble :: Integer -> Integer
squareOfDouble n = (square n) * 2

-- Question 2
doubleSquare :: Integer -> Integer
doubleSquare n = square (square n)

-- Question 3
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Question 4
norm :: Double -> Double -> Double
norm n m = sqrt ( (n * n) + (m * m) )

