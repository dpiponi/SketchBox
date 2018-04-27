{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Points where

import Codec.Picture.ColorQuant
import Control.Monad
import Codec.Picture.Gif
import Codec.Picture.Types
import Data.Int
import Foreign
import System.Environment
import Data.Word
import Data.Time
import Control.Concurrent
import Data.Fixed
-- import Control.Monad.STM
import qualified SDL
import Prelude hiding (init)
import Graphics.Rendering.OpenGL as GL
import SDL.Vect
import Control.Monad.Except
import Control.Monad.State as S hiding (get)
import Control.Lens
import qualified Data.Vector.Storable as SV
import Data.Map.Strict as M
import Sketch

import System.FilePath

import GLCode
import SDLCode

type PointOp a = Either String Program -> Maybe Int -> a

class PointType a where
    drawPoint' :: PointOp (PointOp (SketchMonad ()) -> a)

instance (a ~ ()) => PointType (SketchMonad a) where
    drawPoint' program mn cont = cont program mn

drawPoint :: PointType a => String -> a
drawPoint name = drawPoint' (Left name) Nothing $ \esp mn -> do
    program <- lookupEsp esp
    GL.vertexProgramPointSize GL.$= GL.Enabled
    GL.pointSprite GL.$= Enabled
    GL.blend GL.$= GL.Enabled
    GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    currentProgram $= Just program
    case mn of
        Just n -> io $ GL.drawArrays GL.Points 0 (fromIntegral n)
        Nothing -> error "Don't know array size when drawing points"

-- Float array
instance (PointType r) => PointType (String -> [Float] -> r) where
    drawPoint' = drawPoint'' 1

-- Vector2 array
instance (PointType r, VertexAttribComponent a) => PointType (String -> [GL.Vertex2 a] -> r) where
    drawPoint' = drawPoint'' 2

drawPoint'' :: (PointType r, Storable a) =>
    NumComponents -> PointOp (PointOp (SketchMonad ()) -> String -> [a] -> r)
drawPoint'' size esp mn cont attr values = drawPoint' esp mn $ \esp mn -> do
    program <- lookupEsp esp
    loc <- get $ attribLocation program attr
    vertexAttribArray loc GL.$= Enabled
    io $ withArray values $ \ptr ->
        vertexAttribPointer loc GL.$=
          (ToFloat, VertexArrayDescriptor size Float 0 ptr) -- ToFloat XXX
    let nn = length values
    if mn == Just nn || mn == Nothing
        then do
            cont esp (Just nn)
            vertexAttribArray loc GL.$= Disabled
        else error "Inconsistent array sizes when drawing points"
