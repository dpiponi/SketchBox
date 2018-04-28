module Main where

import qualified Graphics.Rendering.OpenGL as GL
import GLCode
import Sketch
import Points
import Lines

v2f :: Real a => a -> a -> GL.Vertex2 Float
v2f x y = GL.Vertex2 (realToFrac x) (realToFrac y)

main :: IO ()
main = mainLoop $ \time -> do
--main = mainGifLoop "lissa.gif" 12.0 0 1000 $ \time -> do

        let t = 5*realToFrac time :: Float

        GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
        io $ GL.clear [GL.ColorBuffer]

        GL.lineSmooth GL.$= GL.Enabled
        GL.lineWidth GL.$= 2
        GL.hint GL.LineSmooth GL.$= GL.Nicest

        let f z = v2f (0.0095*z*cos ((1.0+0.5*sin (0.02101*t))*z-0.2*t))
                      (0.0095*z*sin (z+0.30812*t)) 

        drawLine "ex3"
                 "vPosition" [f z | z <- [25.0,25.1..100.0]]
                 "fade" [(z-25)/75 :: Float | z <- [25.0, 25.1..100.0]]
        setUniform "glow" "pointSize" (16.0 :: Float)
        drawPoint "glow"
                  "vPosition" (f 100.0)

        io GL.flush
