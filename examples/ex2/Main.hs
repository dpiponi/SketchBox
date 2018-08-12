-- comment
module Main where

import qualified Graphics.Rendering.OpenGL as GL
import Numeric.LinearAlgebra.HMatrix as H hiding (reshape)
import Prelude hiding (init)
import GLCode
import Points

import Sketch

rrand :: Int -> IO (Matrix Double)
rrand n = randn n n

crand :: Int -> IO (Matrix (Complex Double))
crand n = curry toComplex <$> randn n n <*> randn n n

unit :: Floating a => Int -> [a]
unit n = [fromIntegral j/fromIntegral n | j <- [0..n-1]]

initialPoints :: Int -> Vector (Complex Double)
initialPoints num = fromList (n ++ s) where
    n = [(-12.5+25*j) :+ (-20) | j <- unit num]
    s = [(-12.5+25*j) :+ 20 | j <- unit num]

finalPoints :: Int -> Vector (Complex Double)
finalPoints num = fromList (e ++ w) where
    e = [(-20) :+ (-12.5+25*j) | j <- unit num]
    w = [20 :+ (-12.5+25*j) | j <- unit num]

main :: IO ()
main = do
    let num = 20

    u' <- crand (num * 2)
    let u = scale 1.5 u'

    let d = initialPoints num
    let d' = finalPoints num

    let p = diag d
    let p' = u `mul` diag d' `mul` inv u

    mainGifLoop "xxx" 12.0 1 0 100 $ \time -> do
        let t = time/200
        
        let eigs = H.toList $ eigenvalues $ scale (1-realToFrac t) p + scale (realToFrac t) p'

        GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
        io $ GL.clear [GL.ColorBuffer]

        let points = [GL.Vertex2 (0.04*realToFrac x) (0.04*realToFrac y) |
                        x :+ y <- eigs] :: [GL.Vertex2 Float]
        setUniform "shader" "pointSize" (8.0 :: Float)
        drawPoint "shader" "vPosition" points

        io GL.flush
