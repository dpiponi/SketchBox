{-# LANGUAGE TypeApplications #-}

module Main where

import Numeric.LinearAlgebra.HMatrix hiding (scale)
import qualified Graphics.Rendering.OpenGL as GL
import Foreign.Storable
import GLCode
import Sketch
import Points
import Lines

v2f :: Real a => a -> a -> GL.Vertex2 Float
v2f x y = GL.Vertex2 (realToFrac x) (realToFrac y)

translate :: (Fractional a, Storable a) => a -> a -> a -> Matrix a
translate x y z = (4 >< 4) [ 1.0, 0.0, 0.0, 0.0,
                             0.0, 1.0, 0.0, 0.0,
                             0.0, 0.0, 1.0, 0.0,
                             x  , y  , z  , 1.0 ]

scale :: (Fractional a, Storable a) => a -> a -> a -> Matrix a
scale x y z = (4 >< 4) [ x  , 0.0, 0.0, 0.0,
                         0.0, y  , 0.0, 0.0,
                         0.0, 0.0, z  , 0.0,
                         0.0, 0.0, 0.0, 1.0 ]

main :: IO ()
main = mainLoop $ \time -> do

        let t = 5*realToFrac time :: Float

        GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
        io $ GL.clear [GL.ColorBuffer]

        GL.lineSmooth GL.$= GL.Enabled
        GL.lineWidth GL.$= 2
        GL.hint GL.LineSmooth GL.$= GL.Nicest

        setUniform "turtle"
                   "transform" (ident @Float 4)
        drawLine "turtle"
                 "vPosition" [v2f (-1.0) 0.0, v2f 1.0 0.0]

        setUniform "turtle"
                   "transform" (scale 0.5 0.5 0.5 `mul` translate @Float 0.1 0.1 0.0)
        drawLine "turtle"
                 "vPosition" [v2f (-1.0) 0.0, v2f 1.0 0.0]

        io GL.flush
