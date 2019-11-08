import Data.List
import Data.Char

mySentence :: String
mySentence = "Hello people my people people you you people like people"

lowerise :: String -> String
lowerise s = [toLower c | c <- s]

longestWord :: String -> String
longestWord s = last [ y | (x,y) <- sort (zip ([length x | x <- words s]) (words s))]

histogram :: String -> String
histogram sentence = unlines [(replicate ((length (longestWord sentence)) - (length (head x))) ' ') ++ lowerise (head x) ++ " " ++ (replicate (length x) '*') | x <- group (sort (words sentence))]