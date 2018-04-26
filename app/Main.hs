-- comment
module Main where

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import Numeric.LinearAlgebra.HMatrix as H hiding (reshape)
import Data.Array as A
import Prelude hiding (init)
import Control.Monad.State as S
import GLCode
import Control.Lens
import qualified Data.Map.Strict as M

import Sketch

rrand :: Int -> IO (Matrix Double)
rrand n = randn n n

crand :: Int -> IO (Matrix (Complex Double))
crand n = curry toComplex <$> randn n n <*> randn n n

unit :: Floating a => Int -> [a]
unit n = [fromIntegral i/fromIntegral n | i <- [0..n-1]]

initialPoints :: Int -> Vector (Complex Double)
initialPoints num = fromList (n ++ s) where
    n = [(-12.5+25*i) :+ (-20) | i <- unit num]
    s = [(-12.5+25*i) :+ 20 | i <- unit num]

finalPoints :: Int -> Vector (Complex Double)
finalPoints num = fromList (e ++ w) where
    e = [(-20) :+ (-12.5+25*i) | i <- unit num]
    w = [20 :+ (-12.5+25*i) | i <- unit num]

main :: IO ()
main = do
    let num = 20
    let m = 200

    u' <- crand (num * 2)
    let u = scale 1.5 u'

    let d = initialPoints num
    let d' = finalPoints num

    let p = diag d
    let p' = u `mul` diag d' `mul` inv u

    mainGifLoop $ \time -> do

        let t = if time < 0 then 0.0 else if time >= 200 then 1.0 else time/200
        
        let eigs = H.toList $ eigenvalues $ scale (1-realToFrac t) p + scale (realToFrac t) p'

        GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
        io $ GL.clear [GL.ColorBuffer]

        let points = [GL.Vertex2 (0.04*realToFrac x) (0.04*realToFrac y) |
                        x :+ y <- eigs] :: [GL.Vertex2 Float]
        drawPoint "shader" "vPosition" points "pointSize" (8.0 :: Float)

        io GL.flush
