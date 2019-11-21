data Colour = Red | Green | Blue | Yellow | Cyan | Magenta | Black | White deriving Show

-- getInverse :: Colour -> Colour
-- getInverse c 
--     | c == Red = Cyan
--     | c == Green = Magenta
--     | c == Blue = Yellow
--     | c == Yellow = Blue
--     | c == Cyan = Red
--     | c == Magenta = Green
--     | c == Black = White
--     | c == White = Black

rossGetInverse :: Colour -> Colour
rossGetInverse Red = Cyan
rossGetInverse Cyan = Red
rossGetInverse Blue = Yellow
rossGetInverse Yellow = Blue
rossGetInverse Green = Magenta
rossGetInverse Magenta = Green
rossGetInverse Black = White
rossGetInverse White = Black