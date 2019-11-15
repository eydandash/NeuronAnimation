module MyAnimation where
import Animation

picture :: Animation
picture = withBorder (always black) (always 1) ((withoutPaint (rect (always 800) (always 600))))
<<<<<<< HEAD
       `plus`
       withPaint (always (hsl 0 0 0.5)) (neuron 400 170 3 260)
       `plus`
       withPaint (always black)  (rotatedNeuron 530 300)
       `plus`
       drawMovingCircle [(95, -100), (95, 0), (96, 175), (225, 300), (575, 300), (705, 178), (705, -100)] (always red) 30
       `plus`
       drawMovingCircle [(95, 700), (95, 900), (96, 425), (225, 300), (575, 300), (705, 415), (705, 700)] (always red) 30
       `plus`
       drawMovingCircle [(810, 175), (705, 178), (575, 300), (225, 300), (96, 175), (0, 175), (0, -250), (-200, -200), (900, -250)] (always red) 30
       `plus`
       drawMovingCircle (reverse [(810, 175), (705, 178), (575, 300), (225, 300), (96, 175), (0, 175), (0, -250), (-200, -200), (900, -250)]) (always red) 30
       `plus`
       drawMovingCircle (reverse [(95, -100), (95, 0), (96, 175), (225, 300), (575, 300), (705, 178), (705, -100)]) (always red) 30
       `plus`
       drawMovingCircle (reverse [(95, 700), (95, 900), (96, 425), (225, 300), (575, 300), (705, 415), (705, 700)]) (always red) 30
       `plus`
       combine (map drawEmptyCircle [(400, 120, 30, (hsl 0 0 0.5)),
         (400, 480, 30, (hsl 0 0 0.5)),
         (225, 300, 30, black),
         (575, 300, 30, black),
         (525, 605, 30, (hsl 0 0 0.5)),
         (277, 605, 30, (hsl 0 0 0.5)),
         (283, 0, 21, (hsl 0 0 0.5)),
         (520, 0, 21, (hsl 0 0 0.5)),
         (705, 415, 30, black),
         (705, 178, 30, black),
         (96, 425, 30, black),
         (96, 175, 30, black)])
         `plus`
         combine (map drawLine [(380, 500, 3, 100, 45, (hsl 0 0 0.5)),
         (420, 500, 3, 100, 315, (hsl 0 0 0.5)),
         (670, 210, 3, 100, 45, black),
         (600, 315, 3, 100, 315, black),
         (200, 320, 3, 100, 45, black),
         (130, 210, 3, 100, 315, black),
         (490, 25, 3, 100, 45, (hsl 0 0 0.5)),
         (310, 25, 3, 100, 315, (hsl 0 0 0.5)),
         (310, 0, 3, 25, 0, (hsl 0 0 0.5)),
         (312, 23, 3, 50, 90,  (hsl 0 0 0.5)),
         (490, 0, 3, 25, 0 , (hsl 0 0 0.5)),
         (540, 23, 3, 50, 90,  (hsl 0 0 0.5)),
         (130, 390, 3, 50, 0 , black),
         (130, 390, 3, 50, 90,  black),
         (130, 160, 3, 50, 0 , black),
         (130, 208, 3, 50, 90,  black),
         (130, 160, 3, 50, 0 , black),
         (130, 208, 3, 50, 90,  black),
         (670, 160, 3, 50, 0 , black),
         (720, 210, 3, 50, 90,  black),
         (670, 380, 3, 50, 0 , black),
         (720, 380, 3, 50, 90,  black),
         (310, 570, 3, 50, 0 , (hsl 0 0 0.5)),
         (312, 570, 3, 50, 90,  (hsl 0 0 0.5)),
         (490, 570, 3, 50, 0 , (hsl 0 0 0.5)),
         (540, 570, 3, 50, 90,  (hsl 0 0 0.5)),
         (810, 410, 3, 75, 90,  black),
         (810, 175, 3, 75, 90,  black),
         (65, 175, 3, 100, 90, black),
         (65, 425, 3, 100, 90, black),
         (90, 455, 3, 300, 0, black),
         (95, 0, 3, 145, 0, black),
         (700, 445, 3, 160, 0, black),
         (700, 0, 3, 148, 0, black)])
         `plus`
         combine (map negativeBranch[(295, 255, (pi / 4), black), (425, 125, (pi / 4), (hsl 0 0 0.5)), (470, 430, (pi / 4), (hsl 0 0 0.5)), (595, 302, (pi / 4), black)])
         `plus`
         combine (map positiveBranch[(200, 300, (pi / 4), black), (330, 430, (pi / 4), (hsl 0 0 0.5)), (375, 125, (pi / 4), (hsl 0 0 0.5)), (500, 255, (pi / 4), black)])

-- This is  hard to put into list of lists map drawMovingCircle [(),  ]
=======
        `plus`
        withPaint (always (hsl 0 0 0.5)) (neuron 400 170 3 260)
        `plus`
        withPaint (always black)  (rotatedNeuron 530 300)
        `plus`
        positiveBranch 200 300 (pi / 4) black
        `plus`
        positiveBranch 330 430 (pi / 4) (hsl 0 0 0.5)
        `plus`
        positiveBranch 375 125 (pi / 4) (hsl 0 0 0.5)
        `plus`
        positiveBranch 500 255 (pi / 4) black
        `plus`
        negativeBranch 295 255 (pi / 4) black
        `plus`
        negativeBranch 425 125 (pi / 4) (hsl 0 0 0.5)
        `plus`
        negativeBranch 470 430 (pi / 4) (hsl 0 0 0.5)
        `plus`
        negativeBranch 595 302 (pi / 4) black
        `plus`
        drawEmptyCircle 400 120 30 (hsl 0 0 0.5)
        `plus`
        drawEmptyCircle 400 480 30 (hsl 0 0 0.5)
        `plus`
        drawEmptyCircle 225 300 30 black
        `plus`
        drawEmptyCircle 575 300 30 black
        `plus`
        drawLine 380 500 3 100 45 (hsl 0 0 0.5)
        `plus`
        drawLine 420 500 3 100 315 (hsl 0 0 0.5)
        `plus`
        drawLine 670 210 3 100 45 black
        `plus`
        drawLine 600 315 3 100 315 black
        `plus`
        drawLine 200 320 3 100 45 black
        `plus`
        drawLine 130 210 3 100 315 black
        `plus`
        drawLine 490 25 3 100 45 (hsl 0 0 0.5)
        `plus`
        drawLine 310 25 3 100 315 (hsl 0 0 0.5)
        `plus`
        drawLine 310 0 3 25 0 (hsl 0 0 0.5)
        `plus`
        drawLine 312 23 3 50 90 (hsl 0 0 0.5)
        `plus`
        drawLine 490 0 3 25 0 (hsl 0 0 0.5)
        `plus`
        drawLine 540 23 3 50 90 (hsl 0 0 0.5)
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
        drawLine 310 570 3 50 0 (hsl 0 0 0.5)
        `plus`
        drawLine 312 570 3 50 90 (hsl 0 0 0.5)
        `plus`
        drawLine 490 570 3 50 0 (hsl 0 0 0.5)
        `plus`
        drawLine 540 570 3 50 90 (hsl 0 0 0.5)
        `plus`
        drawEmptyCircle 525 605 30 (hsl 0 0 0.5)
        `plus`
        drawEmptyCircle 277 605 30 (hsl 0 0 0.5)
        `plus`
        drawEmptyCircle 283 0 21 (hsl 0 0 0.5)
        `plus`
        drawEmptyCircle 520 0 21 (hsl 0 0 0.5)
        `plus`
        drawEmptyCircle 705 415 30 black
        `plus`
        drawEmptyCircle 705 178 30 black
        `plus`
        drawEmptyCircle 96 425 30 black
        `plus`
        drawEmptyCircle 96 175 30 black
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
        `plus`
        drawMovingCircle [(95, -100), (95, 0), (96, 175), (225, 300), (575, 300), (705, 178), (705, -100)] (always red) 30
        `plus`
        drawMovingCircle [(95, 700), (95, 900), (96, 425), (225, 300), (575, 300), (705, 415), (705, 700)] (always red) 30
        `plus`
        drawMovingCircle [(810, 175), (705, 178), (575, 300), (225, 300), (96, 175), (0, 175), (0, -250), (-200, -200), (900, -250)] (always red) 30
        `plus`
        drawMovingCircle (reverse [(810, 175), (705, 178), (575, 300), (225, 300), (96, 175), (0, 175), (0, -250), (-200, -200), (900, -250)]) (always red) 30
        `plus`
        drawMovingCircle (reverse [(95, -100), (95, 0), (96, 175), (225, 300), (575, 300), (705, 178), (705, -100)]) (always red) 30
        `plus`
        drawMovingCircle (reverse [(95, 700), (95, 900), (96, 425), (225, 300), (575, 300), (705, 415), (705, 700)]) (always red) 30
        
>>>>>>> origin/master

drawMovingCircle :: [(Double, Double)] -> Varying Colour -> Double -> Animation
drawMovingCircle l c r = translate (cycleSmooth 1 l) (withPaint c (circle (always r)))

<<<<<<< HEAD
drawLine :: (Double, Double, Double, Double, Double, Colour) -> Animation
drawLine (x, y, w, h, a, c) = translate (always (x, y)) (rotate (always a) (withPaint (always c) (rect (always w) (always h))))

positiveBranch :: (Double, Double, Double, Colour) -> Animation
positiveBranch (x, y, a, c) =  translate (always (x + (100 * cos(a)), y)) (rotate (always 45) (withPaint (always c) (rect (always 3) (always 65))) )

negativeBranch :: (Double, Double, Double, Colour) -> Animation
negativeBranch (x, y, a, c) =  translate (always (x - (100 * cos(a)), y)) (rotate (always 315) (withPaint (always c) (rect (always 3) (always 65))) )
=======
positiveBranch :: Double -> Double -> Double -> Colour -> Animation
positiveBranch x y a c =  translate (always (x + (100 * cos(a)), y)) (rotate (always 45) (withPaint (always c) (rect (always 3) (always 65))) )

negativeBranch :: Double -> Double -> Double -> Colour -> Animation
negativeBranch x y a c =  translate (always (x - (100 * cos(a)), y)) (rotate (always 315) (withPaint (always c) (rect (always 3) (always 65))) )
>>>>>>> origin/master

neuron :: Double -> Double -> Double -> Double -> Animation
neuron x y w h  = translate (always (x, y)) (rect (always w) (always h))

rotatedNeuron :: Double -> Double -> Animation
rotatedNeuron x y = translate (always (x, y)) (rotate (always 90) (rect (always 3) (always 260)))

drawEmptyCircle :: (Double, Double, Double, Colour) -> Animation
drawEmptyCircle (x, y, r, c) = translate (always (x, y)) (withBorder (always c) (always 3) (withoutPaint (circle (always r))))

drawFullCircle :: Double -> Double -> Double -> Colour -> Animation
drawFullCircle x y r c = translate (always (x, y)) (withPaint (always c) (circle (always r)))

test :: IO()
test = writeFile "test.svg" (svg 800 600 picture)
