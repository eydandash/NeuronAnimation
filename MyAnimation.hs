module MyAnimation where
import Animation

type circle = Animation

picture :: Animation
picture = withBorder (always black) (always 1) ((withoutPaint (rect (always 800) (always 600))))
        `plus`
        neuron 400 170 3 260 neuronOne
        `plus`
        rotatedNeuron 530 300
        `plus`
        positiveBranch 200 300 (pi / 4)
        `plus`
        positiveBranch 330 430 (pi / 4)
        `plus`
        positiveBranch 375 125 (pi / 4)
        `plus`
        positiveBranch 500 255 (pi / 4)
        `plus`
        negativeBranch 295 255 (pi / 4)
        `plus`
        negativeBranch 425 125 (pi / 4)
        `plus`
        negativeBranch 470 430 (pi / 4)
        `plus`
        negativeBranch 595 302 (pi / 4)
        `plus`
        drawCircle 400 120 30 black
        `plus`
        drawCircle 400 480 30 black
        `plus`
        drawCircle 225 300 30 black
        `plus`
        drawCircle 575 300 30 black
        `plus`
        drawLine 380 500 3 100 45 black
        `plus`
        drawLine 420 500 3 100 315 black
        `plus`
        drawLine 670 210 3 100 45 black
        `plus`
        drawLine 600 315 3 100 315 black
        `plus`
        drawLine 200 320 3 100 45 black
        `plus`
        drawLine 130 210 3 100 315 black
        `plus`
        drawLine 490 25 3 100 45 black
        `plus`
        drawLine 310 25 3 100 315 black
        `plus`
        drawLine 310 0 3 25 0 black
        `plus`
        drawLine 312 23 3 50 90 black
        `plus`
        drawLine 490 0 3 25 0 black
        `plus`
        drawLine 540 23 3 50 90 black
        `plus`
        drawLine 130 390 3 50 0 black
        `plus`
        drawLine 130 390 3 50 90 black
        `plus`
        drawLine 130 160 3 50 0 black
        `plus`
        drawLine 130 208 3 50 90 black
        `plus`
        drawLine 130 160 3 50 0 black
        `plus`
        drawLine 130 208 3 50 90 black
        `plus`
        drawLine 670 160 3 50 0 black
        `plus`
        drawLine 720 210 3 50 90 black
        `plus`
        drawLine 670 380 3 50 0 black
        `plus`
        drawLine 720 380 3 50 90 black
        `plus`
        drawLine 310 570 3 50 0 black
        `plus`
        drawLine 312 570 3 50 90 black
        `plus`
        drawLine 490 570 3 50 0 black
        `plus`
        drawLine 540 570 3 50 90 black
        `plus`
        drawCircle 525 605 30 black
        `plus`
        drawCircle 277 605 30 black
        `plus`
        drawCircle 283 0 21 black
        `plus`
        drawCircle 520 0 21 black
        `plus`
        drawCircle 705 415 30 black
        `plus`
        drawCircle 705 178 30 black
        `plus`
        drawCircle 96 425 30 black
        `plus`
        drawCircle 96 175 30 black
        `plus`
        drawLine 810 410 3 75 90 black
        `plus`
        drawLine 810 175 3 75 90 black
        `plus`
        drawLine 65 175 3 100 90 black
        `plus`
        drawLine 65 425 3 100 90 black
        `plus`
        drawLine 90 455 3 300 0 black
        `plus`
        drawLine 95 0 3 145 0 black
        `plus`
        drawLine 700 445 3 160 0 black
        `plus`
        drawLine 700 0 3 148 0 black



drawLine :: Double -> Double -> Double -> Double -> Double -> Colour -> Animation
drawLine x y w h a c = translate (always (x, y)) (rotate (always a) (withPaint (always c) (rect (always w) (always h))))

positiveBranch :: Double -> Double -> Double -> Animation
positiveBranch x y a =  translate (always (x + (100 * cos(a)), y)) (rotate (always 45) (rect (always 3) (always 65)))

negativeBranch :: Double -> Double -> Double -> Animation
negativeBranch x y a =  translate (always (x - (100 * cos(a)), y)) (rotate (always 315) (rect (always 3) (always 65)))

neuron :: Double -> Double -> Double -> Double -> String -> Animation
neuron x y w h n = translate (always (x, y)) (rect (always w) (always h))

rotatedNeuron :: Double -> Double -> Animation
rotatedNeuron x y = translate (always (x, y)) (rotate (always 90) (rect (always 3) (always 260)))

drawCircle :: Double -> Double -> Double -> Colour -> String -> Animation
drawCircle x y r c n = translate (always (x, y)) (withBorder (always c) (always 3) (withoutPaint (circle (always r))))

test :: IO()
test = writeFile "test.svg" (svg 800 600 picture)