module LectureSix where
import Animation

drawMan :: Double -> Double -> Animation
drawMan x y = 
        translate (always (x, (y-25))) (withPaint (always (rgb (168 / 255) (121 / 255) (150 / 255))) (circle (always 100)))
        `plus`
        translate (always ((x - 50), (y-25))) (withPaint (always (rgb (168 / 255) (121 / 255) (150 / 255))) (rect (always 100) (always 400)))
        `plus`
        translate (always ((x-10), (y + 75))) (withPaint (always (rgb (1 / 255) (121 / 255) (150 / 255))) (rect (always 25) (always 200)))

drawCrossHair :: Animation
drawCrossHair = 
    translate (always (650, 275)) (rect (always 1) (always 100))
    `plus`
    translate (always (600, 325)) (rect (always 100) (always 1))

myPic :: Animation
myPic = (drawMan 650 350)
        `plus`
        (drawMan 300 350)
        `plus`
        (drawMan 0 350)
        `plus`
        (drawMan 950 350)
        `plus`
        (drawMan 1100 350)
        `plus`
        (drawMan 800 350)
        `plus`    
        -- (translate (always (650, 325)) (circle (onceSmooth 0 [(1, 100), (2, 0)])))
        (translate (always (650, 325)) (circle (cycleSmooth 0.5 [100, 0, 0])))
        `plus`
        drawCrossHair


test :: IO()
test = writeFile "test.svg" (svg 1300 630 myPic)