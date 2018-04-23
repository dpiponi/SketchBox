module Main where

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import Numeric.LinearAlgebra.HMatrix as H hiding (reshape)
import Data.Array as A
import Prelude hiding (init)
import Control.Monad.State as S
import GLCode
import Control.Lens

import Sketch

rrand :: Int -> IO (Matrix Double)
rrand n = randn n n

crand :: Int -> IO (Matrix (Complex Double))
crand n = do
    x <- randn n n
    y <- randn n n
    return $ toComplex (x, y)

initialPoints :: Int -> [Complex Double]
initialPoints num = n ++ s where
    n = [(-12.5+25*fromIntegral i/(fromIntegral num)) :+ (-20) | i <- [0, 1 .. num-1]]
    s = [(-12.5+25*fromIntegral i/(fromIntegral num)) :+ 20 | i <- [0, 1 .. num-1]]

finalPoints :: Int -> [Complex Double]
finalPoints num = e ++ w where
    e = [(-20) :+ (-12.5+25*fromIntegral i/(fromIntegral num)) | i <- [0, 1 .. num-1]]
    w = [20 :+ (-12.5+25*fromIntegral i/(fromIntegral num)) | i <- [0, 1 .. num-1]]

main :: IO ()
main = do
    let num = 200
    let m = 200

    u' <- crand (num * 2)
    let u = scale 0.5 u'

    let d = fromList $ initialPoints num
    let d' = fromList $ finalPoints num

    let p = diag d :: Matrix (Complex Double)
    let p' = u `mul` diag d' `mul` inv u :: Matrix (Complex Double)

    initSketch

--    myProgram <- compileProgram "shader2" "litterbox"
    
    mainGifLoop (render p p')

render :: Matrix (Complex Double) -> Matrix (Complex Double) -> Float -> SketchMonad ()
render p p' t' = do
    let t = if t' < 0 then 0.0 else if t' >= 200 then 1.0 else t'/200
    Just program <- use shaderProgram
    
    let eigs = eigenvalues $ scale (1-realToFrac t) p + scale (realToFrac t) p'

    GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
    io $ GL.clear [GL.ColorBuffer]

    let points = [GL.Vertex2 (0.04*realToFrac x) (0.04*realToFrac y) |
                    x :+ y <- H.toList eigs] :: [GL.Vertex2 Float]
    drawPoint program (length points) "vPosition" points "pointSize" (2.0 :: Float)

    io GL.flush
