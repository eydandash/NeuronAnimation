{-
    Authors:
    
    LOGIN ID - NAME - STUDENT CARD ID
    
    ACWH025 - Kaleem Peeroo - 170012463
    ACSD227 - Esraa Dandash - 160040246
-}

module MyAnimation where
    import Animation

    picture :: Animation
    picture = 
        -- The green grass background
        background
        `plus`
        road
        `plus`
        sky
        `plus`
        clouds
        `plus`
        lamp (380,300) (50,600)
        `plus`
        lamp (420,300) (750,600)

    {-
        Constant definitions for later use
    -}

    skyColour :: Colour
    skyColour = hsl 169 0.96 0.47

    cloudColour :: Colour
    cloudColour = hsl 159 0.96 0.79

    -- Greenish colour to show grass
    backgroundColour :: Colour
    backgroundColour = hsl 138 0.96 0.32

    -- Yellowish yellow
    lampColour :: Colour
    lampColour = hsl 166 0 0.44

    -- Lamp travels from start point to end point
    lamp :: Point -> Point -> Animation
    lamp (x1,y1) (x2,y2) = 
        rectangle points width height (always black)
        `plus`
        rectangle lightpoints width lightheights (always yellow)
        where
            width = repeatSmooth 5 [(1, 5), (2, 15), (3,25)]
            height = repeatSmooth 20 [(1, 20), (2, 210), (3,400)]
            lightheights = repeatSmooth 20 [(1, 2), (2, 11), (3,20)]
            points = repeatSmooth (x1,y1) [(1, (x1,y1)), (2, ( ( x1 + x2 ) / 2, ( y1 + y2 ) /2 )), (3, (x2,y2))]
            lightpoints = repeatSmooth (x1,y1) [(1, (x1,y1)), (2, ( ( x1 + x2 ) / 2, ( y1 + y2 ) /2 )), (3, (x2,y2))]

    -- Draws the multiple clouds while varying the speeds and positions. Draws two clouds per list. Amount of clouds can be changed through the amount list.
    clouds :: Animation
    clouds = 
        combine (
            [
                cloud (cycleSmooth (amount/amountFactor) (movePoints 0 (amount * moveFactor) pointList)) 20 40 (always cloudColour)
                |
                amountFactor <- [20,10],
                amount <- [1..2],
                moveFactor <- [5,5,50,50],
                pointList <- [points, reversepoints]
            ]
        )
        where
            points = [(x, 100) | x <- ([100,110..800] ++ [800,790..100])]
            reversepoints = [(x, 100) | x <- ([800,790..(-50)] ++ [-50,-40..800])]
    
    -- Draws a blue rectangle over the top half of the picture representing the sky.
    sky :: Animation
    sky = rectangle (always (0,0)) (always 800) (always 300) (always skyColour)
    
    -- Draws a green rectangle behind the road representing the green grass in the background (or the side of the road).
    background :: Animation
    background = 
        rectangle (always (0, 300)) (always 800) (always 300) (always backgroundColour)
    
    -- Draws a rectangle depending on its input of width, height, colour and can even move depending on the points input.
    rectangle :: Varying Point -> Varying Length -> Varying Length -> Varying Colour -> Animation
    rectangle points width height colour = withPaint colour (translate points (rect width height))

    -- Draws a grey polygon first. Then draws a white polygon on top of it. Then draws two rectangles that go on top of the white polygon which shows an illusion the white polygon being seperated.
    road :: Animation
    road = 
        -- Draws the grey polygon giving the illusion of a road
        withPaint (always grey) (polygon [(380, 300), (420, 300), (600, 600), (200, 600)])
        `plus`
        -- Draws the white polygon on top of the grey one giving illusion of division lines of the road
        withPaint (always white) (polygon [(400, 300), (400, 300), (450, 600), (350, 600)])
        `plus`
        -- Draws rectangle that increases in width and height giving illusion of seperation of division lines of the road
        rectangle firstpoints firstws firsths (always grey)
        `plus`
        -- Draws rectangle that increases in width and height giving illusion of seperation of division lines of the road
        rectangle secondpoints secondws secondhs (always grey)
        where
            firstpoints = repeatSmooth (390, 300) [(0, (390, 300)), (2, (300,600))]
            secondpoints = repeatSmooth (390, 300) [(0, (390, 300)), (1, (390, 300)), (3, (300,600))]
            firstws = repeatSmooth 1 [(0,1), (2,250)]
            firsths = repeatSmooth 1 [(0,1), (2,100)]
            secondws = repeatSmooth 1 [(0,1), (1, 1), (3,250)]
            secondhs = repeatSmooth 1 [(0,1), (1, 1), (3,100)]

    -- Draws an ellipse using the width and height given and cloud can also move depending on Points input.
    cloud :: Varying Point -> Double -> Double -> Varying Colour -> Animation
    cloud points height width colour = 
        withPaint (colour) (translate points (ellipse (always width) (always height)))

    -- Moves the points in a list by adding the corresponding inputs.
    movePoints :: Double -> Double -> [Point] -> [Point]
    movePoints xdif ydif points = 
        [(x+xdif, y+ydif) | (x,y) <- points]