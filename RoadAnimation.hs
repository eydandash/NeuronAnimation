module MyAnimation where
    import Animation

    -- Where the magic happens
    picture :: Animation
    picture = 
        -- The green grass background
        drawBackground
        `plus`
        drawRoad
        `plus`
        drawSky
        `plus`
        clouds
        `plus`
        drawLamp (380,300) (50,600)
        `plus`
        drawLamp (420,300) (750,600)

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

    {-
    
        Title: drawLamp
        Description: Draws a lamp that shows on the edges of the road
        Inputs: Start Point :: Point, End Point :: Point

        Lamp travels from start point to end point

    -}
    drawLamp :: Point -> Point -> Animation
    drawLamp (x1,y1) (x2,y2) = 
        drawRectangle points width height (always black)
        `plus`
        drawRectangle lightpoints width lightheights (always yellow)
        where
            width = repeatSmooth 5 [(1, 5), (2, 15), (3,25)]
            height = repeatSmooth 20 [(1, 20), (2, 210), (3,400)]
            lightheights = repeatSmooth 20 [(1, 2), (2, 11), (3,20)]
            points = repeatSmooth (x1,y1) [(1, (x1,y1)), (2, ( ( x1 + x2 ) / 2, ( y1 + y2 ) /2 )), (3, (x2,y2))]
            lightpoints = repeatSmooth (x1,y1) [(1, (x1,y1)), (2, ( ( x1 + x2 ) / 2, ( y1 + y2 ) /2 )), (3, (x2,y2))]

    {-
    
        Title: clouds
        Description: Draws the clouds in the sky
        Inputs: None

        Draws the multiple clouds while varying the speeds and positions. Draws two clouds per list.
        Amount of clouds can be changed through the amount list.

    -}      
    clouds :: Animation
    clouds = 
        combine (
            [drawCloud (cycleSmooth (amount/20) (movePoints 0 (amount * 5) points)) 20 40 (always cloudColour) | amount <- [1..2]]
            ++
            [drawCloud (cycleSmooth (amount/20) (movePoints 0 (amount * 5) reversepoints)) 20 40 (always cloudColour) | amount <- [1..2]]
            ++
            [drawCloud (cycleSmooth (amount/10) (movePoints 50 (amount * 50) points)) 20 40 (always cloudColour) | amount <- [1..2]]
            ++
            [drawCloud (cycleSmooth (amount/10) (movePoints 50 (amount * 50) reversepoints)) 20 40 (always cloudColour) | amount <- [1..2]]
            )
        where
            points = [(x, 100) | x <- ([y,(y+10)..800] ++ [800,790..(y)])]
            reversepoints = [(x, 100) | x <- ([800,790..(-50)] ++ [-50,-40..800])]
            y = 100
    
    {-
    
        Title: drawSky
        Description: Draws the blue sky.
        Inputs: None

        Draws a blue rectangle over the top half of the picture representing the sky.

    -}  
    drawSky :: Animation
    drawSky = drawRectangle (always (0,0)) (always 800) (always 300) (always skyColour)
    
    {-
    
        Title: drawBackground
        Description: Draws green grass background.
        Inputs: None

        Draws a green rectangle behind the road representing the green grass in the background (or the side of the road).

    -} 
    drawBackground :: Animation
    drawBackground = 
        drawRectangle (always (0, 300)) (always 800) (always 300) (always backgroundColour)
    
    {-
    
        Title: drawRectangle
        Description: Draws a rectangle.
        Inputs: Points :: Varying Point, Width :: Varying Length, Height :: Varying Length, Colour :: Varying Colour

        Draws a rectangle depending on its input of width, height, colour and can even move depending on the points input.

    -}  
    drawRectangle :: Varying Point -> Varying Length -> Varying Length -> Varying Colour -> Animation
    drawRectangle points width height colour = withPaint colour (translate points (rect width height))

    {-
    
        Title: drawRoad
        Description: Draws the road in the middle of the screen.
        Inputs: None

        Draws a grey polygon first. Then draws a white polygon on top of it. Then draws two rectangles that go on top of the white polygon which shows an illusion the white polygon being seperated.

    -}  
    drawRoad :: Animation
    drawRoad = 
        -- Draws the grey polygon giving the illusion of a road
        withPaint (always grey) (polygon [(380, 300), (420, 300), (600, 600), (200, 600)])
        `plus`
        -- Draws the white polygon on top of the grey one giving illusion of division lines of the road
        withPaint (always white) (polygon [(400, 300), (400, 300), (450, 600), (350, 600)])
        `plus`
        -- Draws rectangle that increases in width and height giving illusion of seperation of division lines of the road
        drawRectangle firstpoints firstws firsths (always grey)
        `plus`
        -- Draws rectangle that increases in width and height giving illusion of seperation of division lines of the road
        drawRectangle secondpoints secondws secondhs (always grey)
        where
            firstpoints = repeatSmooth (390, 300) [(0, (390, 300)), (2, (300,600))]
            secondpoints = repeatSmooth (390, 300) [(0, (390, 300)), (1, (390, 300)), (3, (300,600))]
            firstws = repeatSmooth 1 [(0,1), (2,250)]
            firsths = repeatSmooth 1 [(0,1), (2,100)]
            secondws = repeatSmooth 1 [(0,1), (1, 1), (3,250)]
            secondhs = repeatSmooth 1 [(0,1), (1, 1), (3,100)]

    {-
    
        Title: drawCloud
        Description: Draws a cloud
        Inputs: Points :: Varying Point, Height :: Double, Width :: Double, Colour :: Varying Colour

        Draws an ellipse using the width and height given and cloud can also move depending on Points input.

    -}  
    drawCloud :: Varying Point -> Double -> Double -> Varying Colour -> Animation
    drawCloud points height width colour = 
        withPaint (colour) (translate points (ellipse (always width) (always height)))

    {-
    
        Title: movePoints
        Description: Takes a list of points and moves them.
        Inputs: X Difference :: Double, Y Difference :: Double, Points :: [Point]

        Moves the points in a list by adding the corresponding inputs.

    -}  
    movePoints :: Double -> Double -> [Point] -> [Point]
    movePoints xdif ydif points = 
        [(x+xdif, y+ydif) | (x,y) <- points]

    test :: IO()
    test = writeFile "test.svg" (svg 800 600 picture)