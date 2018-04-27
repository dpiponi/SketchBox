-- comment
module Main where

import qualified Graphics.Rendering.OpenGL as GL
import Prelude hiding (init)
import GLCode
import Sketch
import Lines

main :: IO ()
main = mainGifLoop $ \time -> do

        let t = 0.1*realToFrac time :: Float

        GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
        io $ GL.clear [GL.ColorBuffer]

        setUniform "ex3" "pointSize" (8.0 :: Float)
        drawLine "ex3" "vPosition" [
                GL.Vertex2 (cos t) (sin t),
                GL.Vertex2 (0.9*cos t) (0.9*sin t)
            ]

        io GL.flush
