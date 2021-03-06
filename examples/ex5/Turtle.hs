{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Turtle where

import Control.Monad.State
-- import Control.Monad.Trans.Class
import Numeric.LinearAlgebra.HMatrix hiding (scale, (!))
import qualified Graphics.Rendering.OpenGL as GL
import Foreign.Storable
import GLCode
import Sketch
-- import Points
import Lines
import Triangles
import Data.Array
import Debug.Trace

clamp :: Float -> Float -> Float -> Float
clamp a b x | x < a = a
            | x > b = b
            | otherwise = x

lerp :: Float -> Float -> Float -> Float
lerp a b x = (1-x)*a+x*b

linstep :: Float -> Float -> Float -> Float
linstep a b x = clamp 0.0 1.0 ((x-a)/(b-a))

smoothstep :: Float -> Float -> Float -> Float
smoothstep a b x =
    let t = clamp 0.0 1.0 ((x-a)/(b-a))
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
                 in (4 >< 4) [ c, s, 0.0, 0.0,
                               -s, c, 0.0, 0.0,
                               0.0, 0.0, 1.0, 0.0,
                               0.0, 0.0, 0.0, 1.0]

rectangle :: (Float, Float, Float) -> Float -> Float -> SketchMonad ()
rectangle (r, g, b) w h = do
    drawTriangle "turtle"
        "vPosition" (rectangleVertices w h)
        "color" [v4f r g b 1.0, v4f r g b 1.0, v4f r g b 1.0,
                 v4f r g b 1.0, v4f r g b 1.0, v4f r g b 1.0]


drawPath :: (Float, Float, Float) -> [(Float, Float)] -> SketchMonad ()
drawPath (r, g, b) vs = do    
    drawLine "turtle"
        "vPosition" (map (uncurry v2f) vs)
        "color" (replicate (length vs) (v4f r g b 1.0))

plotPath :: (Float, Float, Float) -> Int -> (Float -> (Float, Float)) -> Float -> Float -> SketchMonad ()
plotPath rgb n f t0 t1 = do
    let scale = (t1-t0)/fromIntegral n
    drawPath rgb [f t | i <- [0..n],
                        let t = t0+scale*fromIntegral i]

constRectangle :: (Float, Float, Float) -> Float -> Float -> SketchMonad ()
constRectangle (r, g, b) w h = do
    drawTriangle "turtle"
        "vPosition" (rectangleVertices w h)
        "color" [v4f r g b 1.0, v4f r g b 1.0, v4f r g b 1.0,
                 v4f r g b 1.0, v4f r g b 1.0, v4f r g b 1.0]

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

setTransformPoint :: StateT (Matrix Float) (StateT World IO) ()
setTransformPoint = do
    transform <- get
    lift $ setUniform "turtle_point"
               "transform" transform

arrow :: (Float, Float, Float) -> Float -> Float -> Float -> Float -> SketchMonad ()
arrow (r, g, b) thickness headLength headWidth length = do
--     let thickness = 0.01
    constRectangle (r, g, b) length thickness
    drawTriangle "turtle"
        "vPosition" [v2f (0.5*length) (-0.5*headWidth), v2f (0.5*length+headLength) 0, v2f (0.5*length) (0.5*headWidth)]
        "color" [v4f r g b 1.0, v4f r g b 1.0,
                 v4f r g b 1.0]

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

drawVectorField drawArrow m n dx dy vx vy = drawVectorField' drawArrow m n dx dy $ interpVelocityField vx vy

drawVectorField' drawArrow m n dx dy f =
    grid m n dx dy $ \i j -> do
        let (vx, vy) = f (fromIntegral i) (fromIntegral j)
        let theta = atan2 vy vx
        let r = sqrt (vx*vx+vy*vy)
        save $ do
            modify ((rotation theta) `mul`)
            transform <- get
            lift $ setUniform "turtle"
                              "transform" transform
            lift $ drawArrow r

type ColorMap = Float -> GL.Vertex4 Float

drawDensityField :: ColorMap -> Int -> Int -> Float -> Float -> Array (Int, Int) Float -> StateT (Matrix Float) (StateT World IO) ()
drawDensityField cmap m n dx dy a =
    grid m n dx dy $ \i j -> do
        let ip = (i+1) `mod` m
        let jp = (j+1) `mod` m
        lift $ drawTriangle "turtle"
                            "vPosition" [v2f 0.0 0.0, v2f dx 0.0, v2f dx dy,
                                         v2f 0.0 0.0, v2f dx dy, v2f 0.0 dy]
                            "color"     [cmap (a!(i, j)), cmap (a!(ip, j)), cmap (a!(ip, jp)),
                                         cmap (a!(i, j)), cmap (a!(ip, jp)), cmap (a!(i, jp))]

coolwarm :: Float -> Float -> Float -> GL.Vertex4 Float
coolwarm a b x = 
    let mid = 0.5*(a+b)
    in if x > mid
        then let t = smoothstep mid b x in v4f 1.0 (1.0-t) (1.0-t) 1.0
        else let t = smoothstep mid a x in v4f (1.0-t) (1.0-t) 1.0 1.0

-- Interpolate velocity onto uniform grid from staggered grid
-- Let's say velocity field defined by
-- vx!(x, y) = v (x-0.5) y
-- vy!(x, y) = v x (y-0.5)
--
-- So, for example, (v x y)_x = vx!(x,y)+vx!(x+1,y)
interpVelocity :: Array (Int, Int) Float -> Array (Int, Int) Float -> Array (Int, Int) (Float, Float)
interpVelocity vx vy =
    let (_, (nx, ny)) = bounds vx
    in array ((0, 0), (nx, ny)) [((i, j), (0.5*(vx!(ip, j)+vx!(i, j)), 0.5*(vy!(i, jp)+vy!(i, j)))) |
                                 i <- [0..nx],
                                 j <- [0..ny],
                                 let ip = (i+1) `mod` (nx+1),
                                 let jp = (j+1) `mod` (ny+1)]

interpVelocityField :: Array (Int, Int) Float -> Array (Int, Int) Float -> Float -> Float -> (Float, Float)
interpVelocityField vx vy x y =
    (interpScalarField vx (x+0.5) y, interpScalarField vy x (y+0.5))
--     let (_, (nx, ny)) = bounds vx
--         x = x0+0.5
--         y = y0+0.5
--         ix = floor x `mod` nx
--         iy = floor y `mod` ny
--         ixp = (ix+1) `mod` nx
--         iyp = (iy+1) `mod` ny
--         fx = x-fromIntegral ix
--         fy = y-fromIntegral iy
--         vx00 = lerp (vx!(ix, iy)) (vy!(ixp, iy))
--     in (lerp (vx!(ix, iy)) (vx!(ixp, iy)) fx,
--         lerp (vy!(ix, iy)) (vy!(ix, iyp)) fy)

interpScalarField :: Array (Int, Int) Float -> Float -> Float -> Float
interpScalarField p x y =
    let (_, (nx, ny)) = bounds p
        ix = floor x `mod` nx
        iy = floor y `mod` ny
        ixp = (ix+1) `mod` nx
        iyp = (iy+1) `mod` ny
        fx = x-fromIntegral ix
        fy = y-fromIntegral iy
        p00 = p!(ix, iy)
        p01 = p!(ix, iyp)
        p10 = p!(ixp, iy)
        p11 = p!(ixp, iyp)
        p0 = lerp p00 p01 fy
        p1 = lerp p10 p11 fy
    in lerp p0 p1 fx

divergence :: Array (Int, Int) Float -> Array (Int, Int) Float -> Array (Int, Int) (Float, Float)
divergence vx vy =
    let (_, (nx, ny)) = bounds vx
    in array ((0, 0), (nx, ny)) [((i, j), (vx!(ip, j)-vx!(i, j), vy!(i, jp)-vy!(i, j))) |
                                 i <- [0..nx],
                                 j <- [0..ny],
                                 let ip = (i+1) `mod` (nx+1),
                                 let jp = (j+1) `mod` (ny+1)]

grad :: Array (Int, Int) Float -> (Array (Int, Int) Float, Array (Int, Int) Float)
grad p =
    let (_, (nx, ny)) = bounds p
        vx = array ((0, 0), (nx, ny)) [((i, j), p!(ip, j)-p!(i, j)) |
                                      i <- [0..nx],
                                      j <- [0..ny],
                                      let ip = (i+1) `mod` (nx+1)]
        vy = array ((0, 0), (nx, ny)) [((ix, iy), p!(ix, iyp)-p!(ix, iy)) |
                                      ix <- [0..nx],
                                      iy <- [0..ny],
                                      let iyp = (iy+1) `mod` (ny+1)]
    in (vx, vy)

integrate :: (Float -> Float -> (Float, Float)) -> Float -> Float -> Int -> Float -> [(Float, Float)]
integrate v x y 0 dt = []
integrate v x y n dt = (x, y) : 
    let (vx, vy) = v x y
    in integrate v (x+dt*vx) (y+dt*vy) (n-1) dt

duringAndAfter :: Monad m => Float -> Float -> Float -> (Float -> m ()) -> m ()
duringAndAfter t t0 t1 m = 
    if t < t0
        then return ()
        else if t > t1
            then m 1
            else m ((t-t0)/(t1-t0))

beforeAndDuring :: Monad m => Float -> Float -> Float -> (Float -> m ()) -> m ()
beforeAndDuring t t0 t1 m =
    if t > t1
        then return ()
        else if t < t0
            then m 1
            else m ((t-t1)/(t0-t1))
