import Data.Char

-- Question 1a
oneToHundred :: [Int]
oneToHundred = [n | n <- [1..100]]

-- Question 1b
oneToTwentySquares :: [Int]
oneToTwentySquares = [n*n | n <- [1..20], n*n < 20]

-- Question 1c
divisorsOfHundred :: [Int]
divisorsOfHundred = [n | n <- [1..100], 100 `mod` n == 0]

-- Question 2
tripleAll :: [Int] -> [Int]
tripleAll ns = [3*n | n <- ns]

squareAll :: [Int] -> [Int]
squareAll ns = [n*n | n <- ns]

-- Question 3
capitalize :: String -> String
capitalize ls = [toUpper l | l <- ls]

-- Question 4
capitalizeLetters :: String -> String
capitalizeLetters ls = [toUpper l | l <- ls, isAlpha l]

-- Question 5
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

-- Question 7a
isPalindrome :: String -> Bool
isPalindrome ls = (reverse ls == ls)

-- Question 7b
isReallyPalindrome :: String -> Bool
isReallyPalindrome ls = isPalindrome [toUpper l | l <- ls, isAlpha l]

-- Question 8
backwards :: String -> String
backwards ws = unwords (reverse (words ws))

-- Question 9
reversedLetters :: String -> [Char]
reversedLetters ws = backwards (reverse [ w | w <- ws])

-- Question 10
capitalPositions :: String -> [Int]
capitalPositions words = [x | (x,y) <- (zip [1..(length words)] words), isUpper y]

-- Question 11a
contentilise :: [(String, Int)] -> [String]
contentilise input = [x ++ (replicate (40 - (length x + length (show y))) '.') ++ show y | (x,y) <- input]

-- Question 11b
toc :: [(String, Int)] -> String
toc input = unlines [x | (x) <- contentilise input]

-- Question 12
