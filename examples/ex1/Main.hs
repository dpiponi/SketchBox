-- comment
module Main where

import qualified Graphics.Rendering.OpenGL as GL
import Prelude hiding (init)
import GLCode
import Points

import Sketch

main :: IO ()
main = mainGifLoop "xxx" 12.0 1 0 200 $ \time -> do

        let t = 0.1*realToFrac time :: Float

        GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
        io $ GL.clear [GL.ColorBuffer]

        setUniform "example1" "pointSize" (8.0 :: Float)
        drawPoint "example1" "vPosition" [GL.Vertex2 (cos t) (sin t)]

        io GL.flush
