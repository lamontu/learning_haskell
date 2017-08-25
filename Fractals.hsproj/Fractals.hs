module Fractals where

  -- needed to display the picture in the playground
import Codec.Picture

  -- our line graphics programming interface
import LineGraphics


house :: Path
house = [(300, 750), (300, 450), (270, 450), (500, 200),
         (730, 450), (700, 450), (700, 750)]

door :: Path
door = [(420, 750), (420, 550), (580, 550), (580, 750)]

-- Open ghci, load Fractals and then call as below:
-- writePng "draw.png" (drawPicture 2.0 [(lightgreen, house), (red, door)])

spiralRays :: Float -> Float -> Int -> Colour -> Line -> Picture
spiralRays angle scaleFactor n colour line@(p1, p2)
  | n <= 0    = []
  | otherwise = (colour, [p1, p2]) : spiralRays angle scaleFactor (n - 1) newColour newLine
  where
    -- newColour = fade colour  -- spiralRays.png
    newColour = if (n `mod` 3 == 0) then fade colour else colour -- sipralRays2.png
    newLine = scaleLine scaleFactor (rotateLine angle line)

rotateLine :: Float -> Line -> Line
rotateLine alpha ((x1, y1), (x2, y2))
  = ((x1, y1), (x' + x1, y' + y1))
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x' = x0 * cos alpha - y0 * sin alpha
    y' = x0 * sin alpha + y0 * cos alpha

scaleLine :: Float -> Line -> Line
scaleLine factor ((x1, y1), (x2, y2))
  = ((x1, y1), (x' + x1, y' + y1))
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x' = factor * x0
    y' = factor * y0

fade :: Colour -> Colour
fade (redC, greenC, blueC, opacityC)
  = (redC, greenC, blueC, opacityC - 1)

-- Open ghci, load Fractals and then call as below:
-- let myLine = ((400, 400), (420, 420))
-- writePng "spiralRays.png" (drawPicture 2.0 (spiralRays (pi/7) 1.02 400 red myLine))
-- writePng "spiralRays2.png" (drawPicture 4 (spiralRays (pi/27.2) 1.015 400 red myLine))
