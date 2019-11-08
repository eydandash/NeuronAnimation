import Data.List

dups :: Eq a => [a] -> [a]
dups xs
    | null xs = []
    | otherwise = [x | (x,y) <- zip xs (tail xs), x==y]

type Square = (Integer, Integer)
-- top left corner, width, height
type Claim = (Square, Integer, Integer)

square_one :: Square
square_one = (1,2)

square_two :: Square
square_two = (3,1)

square_three :: Square
square_three = (2,3)

square_four :: Square
square_four = (6,6)

example :: [Claim]
example = [
    (square_one, 4, 6),
    (square_two, 3, 3),
    (square_three, 7, 2),
    (square_four, 3, 2)]

-- Tells you squares in a claim
squares :: Claim -> [Square]
squares ((left, top), w, h) = [(left+x, top+y) | x <- [1..w], y <- [1..h]]

-- Check if square inside claim
inside :: Square -> Claim -> Bool
-- inside sq c = elem sq (squares c)
inside (x,y) ((top, left), width, height) = 
    left < x && x <= left + width && top < y && y <= top + height

-- repeated elements of the list
repetitions :: (Ord a, Eq a) => [a] -> [a]
repetitions xs = [head g | g <- group (sort xs), length g > 1]

overlaps :: [Claim] -> [Square]
overlaps cs = repetitions [s | c <- cs, s <- squares c]

isolated :: [Claim] -> [Claim]
isolated cs = 
    [c | c <- cs, not (or [inside s c | s <- bad])]
    where
        bad = overlaps cs


