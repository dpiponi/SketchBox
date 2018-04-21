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

main :: IO ()
main = do
    let num = 20
    let m = 200

    u' <- crand (num * 2)
    let u = scale 0.5 u'

    let n = [(-10+20*fromIntegral i/(fromIntegral num)) :+ (-20) | i <- [0, 1 .. num-1]]
    let s = [(-10+20*fromIntegral i/(fromIntegral num)) :+ 20 | i <- [0, 1 .. num-1]]
    let e = [(-20) :+ (-10+20*fromIntegral i/(fromIntegral num)) | i <- [0, 1 .. num-1]]
    let w = [20 :+ (-10+20*fromIntegral i/(fromIntegral num)) | i <- [0, 1 .. num-1]]

    let d = fromList $ n++s
    let d' = fromList $ e++w

    let p = diag d :: Matrix (Complex Double)
    let p' = u `mul` diag d' `mul` inv u :: Matrix (Complex Double)

    print $ "d' =" ++ show d'
    print $ "eig =" ++ show (eigenvalues p')

    elts <- forM [0..m-1] $ \i -> do
        let t = fromIntegral i/fromIntegral (m-1)
        return $ eigenvalues $ scale (1-t) p + scale t p'
    let eigs = listArray (0, m-1) elts
    mainGifLoop () (render eigs)

render :: Array Int (H.Vector (Complex Double)) -> Float -> StateT (World ()) IO ()
render e t' = do
    let t = t'
    let clamp i = if i < 0 then 0 else if i >= 200 then 199 else i
    GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
    io $ GL.clear [GL.ColorBuffer]
    Just prog <- use shaderProgram
    let it = floor t --floor (50.0*(t `mod'` 8.0))
    forM (H.toList (e A.! (clamp it))) $ \(x :+ y) -> do
        let p = GL.Vertex2 (0.04*realToFrac x) (0.04*realToFrac y) :: GL.Vertex2 Float
        io $ drawPoint prog "vPosition" [p] "pointSize" (8.0 :: Float)
    io GL.flush
