{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

-- import Prelude hidint (until)
import Control.Monad.State
-- import Control.Monad.Trans.Class
import Numeric.LinearAlgebra.HMatrix hiding (scale, (!))
import qualified Graphics.Rendering.OpenGL as GL
-- import Foreign.Storable
import GLCode
import Sketch
-- import Points
-- import Lines
-- import Triangles
import Data.Array
-- import Debug.Trace
import Turtle
import Triangles

ex5 :: IO ()
ex5 = do
    mainLoopState 512 512 16 (ident @Float 4) $ \time -> do

        put (ident @Float 4)

    --         let t = 5*realToFrac time :: Float

        GL.clearColor GL.$= GL.Color4 1.0 1.0 1.0 1
        io $ GL.clear [GL.ColorBuffer]

        GL.lineSmooth GL.$= GL.Enabled
        GL.lineWidth GL.$= 2
        GL.hint GL.LineSmooth GL.$= GL.Nicest
        GL.multisample GL.$= GL.Enabled
 
        let p = array ((0, 0), (19, 19)) [((ix, iy), 0.25*cos (-0.75*time+2.0*pi*fromIntegral (ix+iy)/10)+
                                                     0.25*sin (0.5*time+3.0*2*pi*(fromIntegral (ix-iy)/20))) |
                                          ix <- [0..19],
                                          iy <- [0..19]]
                          
        translate (-1.0) (-1.0) 0.0
        drawDensityField (coolwarm (-0.45) 0.45) 20 20 0.1 0.1 p
        let (vx, vy) = grad p
--         let vel = fmap (\(x, y) -> (0.1*x, 0.1*y)) $ interpVelocity vx vy
--         drawVectorField 20 20 0.1 0.1 vx vy

--         lift $ drawPath [(x, y) | i <- [0..10], let x = 0.1*fromIntegral i, let y = x*x]

--         let f t = (t, 0.25*t*t)
--         let time' = 0.3*time
--         let d = 3*(time'-fromIntegral @Int (floor time'))
--         lift $ plotPath 100 f 0 d
--        let curve = integrate (\x y ->let (u,v)=interpVelocityField vx vy x y in (10*u,10*v)) 0 0 100 0.01
--         let curve = integrate (\x y ->let (u,v)=interpVelocityField vx vy x y in (u,v)) 1.0 1.0 300 0.01
        duringAndAfter time 5 10 $ \_ ->
            forM_ [-5.0, -3.0..5.0] $ \x -> do
                forM_ [-5.0, -3.0..5.0] $ \y -> do
                    let curve = integrate (interpVelocityField vx vy) (10+x) (10+y) 500 0.1
                    lift $ drawPath (0.0, 0.0, 0.0) [(0.1*u, 0.1*v) | (u, v) <- curve]

        return ()

image1 = do
--     mainLoopState 1024 1024 16 (ident @Float 4) $ \time -> do
    mainGifLoopState "euler_or_lagrange.gif" 12 768 768 16 1 100 (ident @Float 4) $ \time -> do

        put (ident @Float 4)

        GL.clearColor GL.$= GL.Color4 1.0 1.0 1.0 1
        io $ GL.clear [GL.ColorBuffer]

        GL.lineSmooth GL.$= GL.Enabled
        GL.lineWidth GL.$= 2
        GL.hint GL.LineSmooth GL.$= GL.Nicest
        GL.multisample GL.$= GL.Enabled

        translate (-0.95) (-0.90) 0.0
        let vfield x y = (0.01*(7*sin x+sin (7*y)), 0.01*(-sin (10*x-5*y)))
        beforeAndDuring time 12 14 $ \time ->
            drawVectorField' (arrow (1-time, 1-time, 1-time) 0.007 0.03 0.03) 13 13 0.15 0.15 $ \i j ->
                let x = 0.15*fromIntegral i
                    y = 0.15*fromIntegral j
                in vfield x y
--         let curve = integrate vfield 0.2 0.1 200 0.2
--         lift $ drawPath (0.0, 0.0, 0.0) (take 100 curve)
--         lift $ drawPath (0.0, 0.0, 0.0) (drop 99 curve)
        duringAndAfter time 0 5 $ \time ->
            forM_ [0.1, 0.2.. 1.9] $ \starty -> do
                let curve = integrate vfield 0.2 starty (floor (200*time)) 0.2
                lift $ drawPath (0.5, 0.5, 0.5) (take 100 curve)
                lift $ drawPath (0.5, 0.5, 0.5) (drop 99 curve)

main :: IO ()
main = image1
