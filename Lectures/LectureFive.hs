import Data.Char

-- Question 1
{-
    MY ANSWER:
    capitalize :: String -> String
    capitalize ls = map toUpper (filter isLower (ls))
-}

-- ROSS ANSWER:
capitalize :: String -> String
capitalize cs = map toUpper cs

capitals :: String -> String
capitals cs = filter isUpper cs

capitalizeLetters :: String -> String
capitalizeLetters cs = map toUpper (filter isAlpha cs)

