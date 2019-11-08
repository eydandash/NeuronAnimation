import Data.Char

-- Tutorial 2 Exercises
-- Question 1
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z = x /= y && x /= z && y /= z

-- Question 2
phi :: Float
phi = (1 + (sqrt 5)) / 2

-- Question 3
getDecimal :: Double -> Double
getDecimal n = n - (fromIntegral (floor (n)))

-- Question 4
factorial :: Int -> Int
factorial n = product [1..n]

-- Question 5
between :: Int -> Int -> Int -> Bool
between x y z = (y >= x && y <= z) || (y >= z && y <= x)

-- Question 6
charToInt :: Char -> Int
charToInt x = (ord x) - 48

-- Question 7A
isLeapYear :: Int -> Bool
isLeapYear x = (x `mod` 4 == 0)

-- Question 7B
daysInYear :: Int -> Int
daysInYear x 
    | x `mod` 4 == 0 = 366
    | otherwise = 365

-- Question 8
middleNumber :: Int -> Int -> Int -> Int
middleNumber x y z
    | between y x z = x
    | between x y z = y
    | otherwise     = z