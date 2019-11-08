-- Question 4: Use higher order functions to write expressions to compute
{-
    (a) the squares of all the numbers up to 20.
    (b) all the square numbers less than 500.
    (c) all the square numbers between 500 and 1000.
-}
foura :: a -> [Int]
foura a = (map (\x -> x * x) [1..20])

fourb :: a -> [Int]
fourb a = takeWhile (<500) (map (\x -> x * x) [1..])

fourc :: a -> [Int]
fourc a = filter (>500) (takeWhile (<1000) (map (\x -> x * x) [1..]))

-- Question 5
-- Work out what the following function does, and rewrite it using higher order functions and operator sections instead of a list comprehension:
count :: Eq a => a -> [a] -> Int
count x ys = length [y | y <- ys, y == x]

mycount :: Eq a => a -> [a] -> Int
mycount x ys = length (filter (\z -> z==x) ys)

-- Question 6
{-
    Work out the types of the Prelude functions defined by
        const x y = x
        flip f x y = f y x
    What might they be useful for?
-}

-- Question 7
myLength :: [Int] -> Int
myLength ns = sum (map (^0) ns)

-- f :: [Integer] -> [Integer]
-- f = filter ((< 20) . (^2))

f :: [Float] -> [Float]
f xs = map oneOverTwo xs
    where oneOverTwo = (\x -> (incrementOne x)/2)
          incrementOne x = x + 1

{-
    Question 11:
    map (1/) (scanl (*) 1 [1..])
-}

-- Question 12
doubleList :: Int -> [Int]
doubleList 0 = map (*2) [1..]
doubleList n = map (*2) [1..n]

diff :: (Int, Int) -> Int
diff (x, y) = (y-x)

twelve :: Int -> [Bool]
twelve n = [ diffIsN | (x,y) <- diffDoubleList]
    where
        diffDoubleList = (zip (doubleList 10) ( tail (doubleList 10) ))
        diffIsN = ( ( diff (x, y) ) == n )