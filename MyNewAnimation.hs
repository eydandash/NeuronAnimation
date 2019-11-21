module MyAnimation where
    import Animation
    
    backgroundColour :: Colour
    backgroundColour = hsl 162 0.75 0.8

    brown :: Colour
    brown = hsl 13 0.58 0.21

    cloudColour :: Colour
    cloudColour = hsl 216 0.75 0.8

    picture :: Animation
    picture = 
        drawBackground
        `plus`
        drawSky
        `plus`
        drawGrass
        `plus`
        drawRoad
        `plus`
        drawRightTreeLine    
        `plus`
        drawLeftTreeLine

    drawGrass :: Animation
    drawGrass = drawRectangle (400, 450) 800 300 lime
        `plus`
        combine (
            [drawRectangle p 3 10 green | p <- points]
        )
            where
                points = [(x,y) | x <- [0..800], y <- [300..600], (ceiling x) `mod` 10 == 0, (ceiling y) `mod` 30 == 0]

    drawSky :: Animation
    drawSky = 
        drawCloud (cycleSmooth 0.2 [(x, 30) | x <- [0..800], (ceiling x) `mod` 57 == 0])
        `plus`
        drawCloud (cycleSmooth 0.5 [(x, 50) | x <- [0..800], (ceiling x) `mod` 57 == 0])
        `plus`
        drawCloud (cycleSmooth 0.2 [(x, 90) | x <- [800,799..0], (ceiling x) `mod` 57 == 0])
        `plus`
        drawCloud (cycleSmooth 0.4 [(x, 20) | x <- [800,799..0], (ceiling x) `mod` 57 == 0])

    drawCloud :: Varying Point -> Animation
    drawCloud p = withPaint (always cloudColour) (translate (p) (ellipse (always 50) (always 25)))

    drawRightTreeLine :: Animation
    drawRightTreeLine = 
        drawTree (400, 300) (hsl 101 0.58 0.21) (10+(x/9)) (50 + (x*1.5))
        where
            x = cycleSmooth 0.5 [x | x <- [10..400], ceiling x `mod` 45 == 0]
        -- combine (
        --     [ 
        --         drawTree (400 + x, 300) (hsl 101 0.58 0.21) (10 + ( x/9 ) ) ( 50 + ( x * 1.5 ) ) 
        --         | x <- [10..400], ceiling x `mod` 45 == 0 
        --     ]
        -- )

    drawLeftTreeLine :: Animation
    drawLeftTreeLine = 
        combine (
            [ 
                drawTree (400 - x, 300) (hsl 101 0.58 0.21) (always (10 + ( x/10 ) )) (always ( 50 + ( x * 1.5 ) )) 
                | x <- [10..400], ceiling x `mod` 45 == 0
            ]
        )

    drawRoad :: Animation
    drawRoad = 
        withPaint (always grey) (polygon [ (400,300), (400,300), (0,600), (800,600) ])
        `plus`
        withPaint (always white) ( polygon [x | x <- [(400,305), (250,600), (550,600)]] )
        `plus`
        drawMovingRoad

    drawMovingRoad :: Animation
    drawMovingRoad = 
        translate 
        (
            cycleSmooth 0.4
            [ x | x <- [(400-(z/2),y+(z/2)) | y <- [300..1000], (ceiling y) `mod` 50 == 0, z <- widthValues]]
        ) 
    
        (
        
            withPaint (always grey) (
            rect 
            (  
                cycleSmooth 0.4 widthValues
            ) 
            (
                cycleSmooth 0.4 heightValues
            ))
        )
        where
            widthValues = [ y | y <- [x | x <- [0..400], (ceiling x) `mod` 29 == 0]]
            heightValues = [ y | y <- [x | x <- [0..100], (ceiling x) `mod` 7 == 0]]

    drawTree :: Point -> Colour -> Varying Double -> Varying Double -> Animation
    drawTree (x,y) c w h =
        drawTreeTrunk (x,y) w h
        `plus`
        drawTreeLeaves (x,y) c w h

    drawTreeLeaves :: Point -> Colour -> Varying Double -> Varying Double -> Animation
    drawTreeLeaves (x,y) c w h = withBorder (always green) (always 1) (withPaint (always c) (translate (always ( x, (y - ((always h)/2)) )) ( ellipse ((always w)*1.2) (h/2) )))
        
    drawTreeTrunk :: Point -> Varying Double -> Varying Double -> Animation
    drawTreeTrunk (x,y) w h = drawRectangle (x,y) w h brown

    drawPoint :: Point -> Animation
    drawPoint (x,y) = translate (always (x,y)) ( circle (always 2) )

    drawBackground :: Animation
    drawBackground = withPaint (always backgroundColour) (drawRectangle (400, 300) (always 800) (always 600) backgroundColour)

    drawRectangle :: Point -> Varying Double -> Varying Double -> Colour -> Animation
    drawRectangle (x,y) w h c = translate ( always point ) ( withPaint (always c) (rect (always w) (always h)))
        where  
            point = ( x - ( w/2 ), y - ( h/2 ) )

    test :: IO()
    test = writeFile "test.svg" (svg 800 600 picture)
    