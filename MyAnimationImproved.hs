module MyAnimation where
    import Animation
    import Data.Char
    import Data.List
    
    alwaysRed :: Varying Colour
    alwaysRed = always red
    
    alwaysGreen :: Varying Colour
    alwaysGreen = always green

    picture :: Animation
    picture = 
            drawBorder
            `plus`
            combine (map drawNeuron rowOne)
            `plus`
            combine (map drawNeuron rowTwo)
            `plus`
            combine (map drawNeuron rowThree)
            `plus`
            combine (map drawNeuron rowFour)
            `plus`
            combine (map drawNeuron rowFive)
            `plus`
            combine (map drawNeuron rowSix)
            `plus`
            drawMovingCircle [(-50, 50), (-50, 50), (850, 51), (850, 51)] alwaysRed 20
            `plus`
            drawMovingCircle [(-50, 250), (-50, 250), (850, 251), (850, 251)] alwaysRed 20
            `plus`
            drawMovingCircle [(-50, 450), (-50, 450), (850, 451), (850, 451)] alwaysRed 20
            `plus`
            drawMovingCircle [(-50, 150), (850, 151), (850, 151)] alwaysGreen 20
            `plus`
            drawMovingCircle [(-50, 350), (850, 351), (850, 351)] alwaysGreen 20
            `plus`
            drawMovingCircle [(-50, 550), (850, 551), (850, 551)] alwaysGreen 20
                where 
                    rowOne = [((x + 30, 50), ((x + 80), 51)) | x <- [-100..800], ceiling x `mod` 50 == 0]
                    rowTwo = [((x + 30, 150), ((x + 80), 151)) | x <- [-100..800], ceiling x `mod` 50 == 0]
                    rowThree = [((x + 30, 250), ((x + 80), 251)) | x <- [-100..800], ceiling x `mod` 50 == 0]
                    rowFour = [((x + 30, 350), ((x + 80), 351)) | x <- [-100..800], ceiling x `mod` 50 == 0]
                    rowFive = [((x + 30, 450), ((x + 80), 451)) | x <- [-100..800], ceiling x `mod` 50 == 0]
                    rowSix = [((x + 30, 550), ((x + 80), 551)) | x <- [-100..800], ceiling x `mod` 50 == 0]
    
    drawNeuron :: (Point, Point) -> Animation
    drawNeuron ((x1, y1), (x2, y2)) = 
        drawFullCircle x1 y1 (dist / 5) black
        -- `plus`
        -- drawFullCircle x2 y2 3 black
        `plus`
        drawLine (x1, y1, 3, dist, angle, black)
        `plus`
        drawLine (x2, y2, 3, dist / 3, angle + 45, black)
        `plus`
        drawLine (x2, y2, 3, dist / 3, angle - 45, black)
            where
                dist = calculateDistance (x1, y1) (x2, y2)
                angle = calcAngleToAxis (x1, y1) (x2, y2)

    calcAngleToAxis :: Point -> Point -> Angle
    calcAngleToAxis (x1, y1) (x2, y2) | angle > 90 = angle
                                      | angle < 90 = 90 - angle
                where
                    xdif = (max x1 x2) - (min x1 x2)
                    ydif = (max y1 y2) - (min y1 y2)
                    angle = (atan2 xdif ydif) / pi * 180

    calculateDistance :: Point -> Point -> Length
    calculateDistance (x1, y1) (x2, y2) = sqrt ( (xdif * xdif) + (ydif * ydif) )
        where 
            xdif = (max x1 x2) - (min x1 x2)
            ydif = (max y1 y2) - (min y1 y2)

    drawMovingCircle :: [(Double, Double)] -> Varying Colour -> Double -> Animation
    drawMovingCircle l c r = translate (cycleSmooth 1 l) (withPaint c (circle (always r)))
    
    drawLine :: (Double, Double, Double, Double, Double, Colour) -> Animation
    drawLine (x, y, w, h, a, c) = translate (always (x, y)) (rotate (always a) (withPaint (always c) (rect (always h) (always w))))
        
    drawFullCircle :: Double -> Double -> Double -> Colour -> Animation
    drawFullCircle x y r c = translate (always (x, y)) (withPaint (always c) (circle (always r)))
    
    drawBorder :: Animation
    drawBorder = withBorder (always black) (always 1) ((withoutPaint (rect (always 800) (always 600))))

    test :: IO()
    test = writeFile "test.svg" (svg 800 600 picture)
    