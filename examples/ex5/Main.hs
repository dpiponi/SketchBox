{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State
-- import Control.Monad.Trans.Class
import Numeric.LinearAlgebra.HMatrix hiding (scale, (!))
import qualified Graphics.Rendering.OpenGL as GL
import Foreign.Storable
import GLCode
import Sketch
-- import Points
-- import Lines
import Triangles
import Data.Array

clamp :: Float -> Float -> Float -> Float
clamp a b x | x < a = a
            | x > b = b
            | otherwise = x

lerp :: Float -> Float -> Float -> Float
lerp a b x = (1-x)*a+x*b

linstep :: Float -> Float -> Float -> Float
linstep a b x = clamp 0.0 1.0 ((x-a)/(x-b))

smoothstep :: Float -> Float -> Float -> Float
smoothstep a b x =
    let t = clamp 0.0 1.0 ((x-a)/(x-b))
    in t*t*(3.0-2.0*t)

v2f :: Real a => a -> a -> GL.Vertex2 Float
v2f x y = GL.Vertex2 (realToFrac x) (realToFrac y)

v3f :: Real a => a -> a -> a -> GL.Vertex3 Float
v3f x y z = GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

v4f :: Real a => a -> a -> a -> a -> GL.Vertex4 Float
v4f x y z w = GL.Vertex4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w)

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
        "vPosition" (rectangleVertices w h)
        "color" [v4f 1.0 0.0 0.0 1.0, v4f 0.0 1.0 0.0 1.0, v4f 0.0 0.0 1.0 1.0,
                 v4f 1.0 0.0 0.0 1.0, v4f 0.0 1.0 0.0 1.0, v4f 0.0 0.0 1.0 1.0]

constRectangle :: Float -> Float -> SketchMonad ()
constRectangle w h = do
    drawTriangle "turtle"
        "vPosition" (rectangleVertices w h)
        "color" [v4f 0.0 0.0 0.0 1.0, v4f 0.0 0.0 0.0 1.0, v4f 0.0 0.0 0.0 1.0,
                 v4f 0.0 0.0 0.0 1.0, v4f 0.0 0.0 0.0 1.0, v4f 0.0 0.0 0.0 1.0]

rectangleVertices :: Float -> Float -> [GL.Vertex2 Float]
rectangleVertices w h =
        [v2f (-0.5*w) (-0.5*h), v2f (0.5*w) (-0.5*h), v2f (0.5*w) (0.5*h),
         v2f (-0.5*w) (-0.5*h), v2f (0.5*w) (0.5*h), v2f (-0.5*w) (0.5*h)]

rotate :: (Floating t, Numeric t, MonadState (Matrix t) m) => t -> m ()
rotate angle = modify (rotation angle `mul`)

scale :: (Fractional t, Numeric t, MonadState (Matrix t) m) => t -> t -> t -> m ()
scale s t u = modify (scaling s t u `mul`)

translate :: (Fractional t, Numeric t, MonadState (Matrix t) m) => t -> t -> t -> m ()
translate u v w = modify (translation u v w `mul`)

save :: StateT (Matrix Float) (StateT World IO) () -> StateT (Matrix Float) (StateT World IO) ()
save c = do
    m <- get
    a <- c
    put m
    setTransform
    return a

setTransform :: StateT (Matrix Float) (StateT World IO) ()
setTransform = do
    transform <- get
    lift $ setUniform "turtle"
               "transform" transform

arrow :: Float -> SketchMonad ()
arrow theta = do
    constRectangle 0.1 0.02
    drawTriangle "turtle"
        "vPosition" [v2f 0.05 (-0.02), v2f 0.1 0, v2f 0.05 0.02]
        "color" [v4f 0.0 0.0 0.0 1.0, v4f 0.0 0.0 0.0 1.0,
                 v4f 0.0 0.0 0.0 1.0]

grid :: Int -> Int -> Float -> Float ->
        (Int -> Int -> StateT (Matrix Float) (StateT World IO) ()) ->
        StateT (Matrix Float) (StateT World IO) ()
grid m n dx dy f =
    forM_ [0..(m-1)] $ \i ->
        forM_ [0..(n-1)] $ \j -> save $ do
            let x = dx*fromIntegral i
            let y = dy*fromIntegral j
            translate x y 0
            setTransform
            f i j

--drawVectorField :: Int -> Int -> Float -> Float -> (Array (Int, Int) Float)
drawVectorField m n dx dy a =
    grid m n dx dy $ \i j -> do
        let theta = a!(i, j)
        save $ do
            modify ((rotation theta) `mul`)
            transform <- get
            lift $ setUniform "turtle"
                              "transform" transform
            lift $ arrow 0

type ColorMap = Float -> GL.Vertex4 Float

drawDensityField :: ColorMap -> Int -> Int -> Float -> Float -> Array (Int, Int) Float -> StateT (Matrix Float) (StateT World IO) ()
drawDensityField cmap m n dx dy a =
    grid m n dx dy $ \i j -> do
    lift $ drawTriangle "turtle"
                        "vPosition" [v2f 0.0 0.0, v2f dx 0.0, v2f dx dy,
                                     v2f 0.0 0.0, v2f dx dy, v2f 0.0 dy]
                        "color"     [cmap (a!(i, j)), cmap (a!(i+1, j)), cmap (a!(i+1, j+1)),
                                     cmap (a!(i, j)), cmap (a!(i+1, j+1)), cmap (a!(i, j+1))]

coolwarm :: Float -> Float -> Float -> GL.Vertex4 Float
coolwarm a b x = 
    let mid = 0.5*(a+b)
    in if x > mid
        then let t = linstep mid b x in v4f 1.0 (1.0-t) (1.0-t) 1.0
        else let t = linstep mid a x in v4f (1.0-t) (1.0-t) 1.0 1.0

main :: IO ()
main = mainLoopState 16 (ident @Float 4) $ \time -> do

        put (ident @Float 4)

--         let t = 5*realToFrac time :: Float

        GL.clearColor GL.$= GL.Color4 1.0 1.0 1.0 1
        io $ GL.clear [GL.ColorBuffer]

        GL.lineSmooth GL.$= GL.Enabled
        GL.lineWidth GL.$= 2
        GL.hint GL.LineSmooth GL.$= GL.Nicest
        GL.multisample GL.$= GL.Enabled

        -- modify (rotation (0.1*time) `mul`)

        let a = array ((0, 0), (4, 4)) [((i, j), 0.2*time*fromIntegral i) | i <- [0..4], j <- [0..4]]
        let b = array ((0, 0), (4, 4)) [((i, j), 0.5*fromIntegral (i-j)) | i <- [0..4], j <- [0..4]]
                          
        drawDensityField (coolwarm (-0.5) 0.5) 4 4 0.2 0.2 b
        drawVectorField 5 5 0.2 0.2 a

--                 let theta = 0.2*time*fromIntegral i
--                 modify ((rotation theta) `mul`)
--                 transform <- get
--                 lift $ setUniform "turtle"
--                                   "transform" transform
--                 lift $ arrow 0
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
