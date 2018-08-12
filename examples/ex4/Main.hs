{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.State
import Control.Monad.Trans.Class
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

rotate :: (Fractional a, Storable a, Floating a) => a -> Matrix a
rotate theta = let c = cos theta
                   s = sin theta
               in (4 >< 4) [ c, -s, 0.0, 0.0,
                             s, c, 0.0, 0.0,
                             0.0, 0.0, 1.0, 0.0,
                             0.0, 0.0, 0.0, 1.0]

translateBy :: Float -> Float -> Float -> StateT (Matrix Float) (StateT World IO) ()
translateBy x y z = modify (translate x y z `mul`) 

scaleBy :: Float -> StateT (Matrix Float) (StateT World IO) ()
scaleBy s = modify (scale s s s `mul`)

rotateBy :: Float -> StateT (Matrix Float) (StateT World IO) ()
rotateBy s = modify (rotate s `mul`)

main :: IO ()
main = mainLoopState 1 (ident @Float 4) $ \time -> do

        put (ident @Float 4)

        let t = 5*realToFrac time :: Float

        GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
        io $ GL.clear [GL.ColorBuffer]

        GL.lineSmooth GL.$= GL.Enabled
        GL.lineWidth GL.$= 2
        GL.hint GL.LineSmooth GL.$= GL.Nicest

        modify (rotate (0.1*time) `mul`)
        replicateM_ 23 $ do
            let r = 10+5*sin (0.71*time)
            rotateBy (pi/r)
            let s = sin (0.9*time)
            scaleBy s
            translateBy 0.01 0.01 0.0
            transform <- get

            lift $ setUniform "turtle"
                       "transform" transform
            lift $ drawLine "turtle"
                     "vPosition" [v2f (-1.0) 0.0, v2f 1.0 0.0]

            io GL.flush
