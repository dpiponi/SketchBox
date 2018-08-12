{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State
-- import Control.Monad.Trans.Class
import Numeric.LinearAlgebra.HMatrix hiding (scale)
import qualified Graphics.Rendering.OpenGL as GL
import Foreign.Storable
import GLCode
import Sketch
-- import Points
-- import Lines
import Triangles

v2f :: Real a => a -> a -> GL.Vertex2 Float
v2f x y = GL.Vertex2 (realToFrac x) (realToFrac y)

translation :: (Fractional a, Storable a) => a -> a -> a -> Matrix a
translation x y z = (4 >< 4) [ 1.0, 0.0, 0.0, 0.0,
                               0.0, 1.0, 0.0, 0.0,
                               0.0, 0.0, 1.0, 0.0,
                               x  , y  , z  , 1.0 ]

scaling :: (Fractional a, Storable a) => a -> a -> a -> Matrix a
scaling x y z = (4 >< 4) [ x  , 0.0, 0.0, 0.0,
                           0.0, y  , 0.0, 0.0,
                           0.0, 0.0, z  , 0.0,
                           0.0, 0.0, 0.0, 1.0 ]

rotation :: (Fractional a, Storable a, Floating a) => a -> Matrix a
rotation theta = let c = cos theta
                     s = sin theta
                 in (4 >< 4) [ c, -s, 0.0, 0.0,
                               s, c, 0.0, 0.0,
                               0.0, 0.0, 1.0, 0.0,
                               0.0, 0.0, 0.0, 1.0]

rectangle :: Float -> Float -> SketchMonad ()
rectangle w h = do
    drawTriangle "turtle"
        "vPosition" [v2f (-0.5*w) (-0.5*h), v2f (0.5*w) (-0.5*h), v2f (0.5*w) (0.5*h),
                     v2f (-0.5*w) (-0.5*h), v2f (0.5*w) (0.5*h), v2f (-0.5*w) (0.5*h)]

rotate :: (Floating t, Numeric t, MonadState (Matrix t) m) => t -> m ()
rotate angle = modify (rotation angle `mul`)

scale :: (Fractional t, Numeric t, MonadState (Matrix t) m) => t -> t -> t -> m ()
scale s t u = modify (scaling s t u `mul`)

translate :: (Fractional t, Numeric t, MonadState (Matrix t) m) => t -> t -> t -> m ()
translate u v w = modify (translation u v w `mul`)

save :: MonadState s m => m a -> m a
save c = do
    m <- get
    a <- c
    put m
    return a

setTransform :: StateT (Matrix Float) (StateT World IO) ()
setTransform = do
    transform <- get
    lift $ setUniform "turtle"
               "transform" transform

arrow :: Float -> SketchMonad ()
arrow theta = do
    rectangle 0.1 0.02
    drawTriangle "turtle"
        "vPosition" [v2f 0.05 (-0.02), v2f 0.1 0, v2f 0.05 0.02]

grid :: (MonadState (Matrix Float) m) =>
        Int -> Int -> Float -> Float -> m () -> m ()
grid m n dx dy f =
    forM_ [0..(m-1)] $ \i ->
        forM_ [0..(n-1)] $ \j -> save $ do
            let x = dx*fromIntegral i
            let y = dy*fromIntegral j
            translate x y 0
            f

main :: IO ()
main = mainLoopState (ident @Float 4) $ \time -> do

        put (ident @Float 4)

--         let t = 5*realToFrac time :: Float

        GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
        io $ GL.clear [GL.ColorBuffer]

        GL.lineSmooth GL.$= GL.Enabled
        GL.lineWidth GL.$= 2
        GL.hint GL.LineSmooth GL.$= GL.Nicest

        modify (rotation (0.1*time) `mul`)

        grid 5 5 0.2 0.2 $ do
            transform <- get
            lift $ setUniform "turtle"
                              "transform" transform
            lift $ arrow 0
--         replicateM_ 23 $ do
--             let r = 10+5*sin (0.71*time)
--             modify (rotation (pi/r) `mul`)
--             let s = sin (0.9*time)
--             modify (scaling s s s `mul`)
--             modify (translation 0.01 0.02 0.0 `mul`)
--             transform <- get
-- 
--             lift $ setUniform "turtle"
--                        "transform" transform
--             lift $ arrow
-- 
--             io GL.flush
