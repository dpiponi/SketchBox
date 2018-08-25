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
