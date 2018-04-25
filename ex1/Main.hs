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

main :: IO ()
main = do
    mainGifLoop $ \time -> do

        let t = 0.1*realToFrac time :: Float

        GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
        io $ GL.clear [GL.ColorBuffer]

        drawPoint "example1" 1 "vPosition" [GL.Vertex2 (cos t) (sin t)] "pointSize" (8.0 :: Float)

        io GL.flush
