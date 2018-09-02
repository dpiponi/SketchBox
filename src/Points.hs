{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Points where

import Data.Maybe
import Foreign
import Graphics.Rendering.OpenGL as GL
import Sketch
import GLCode

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

-- A vector2
instance (PointType r, VertexAttribComponent a) => PointType (String -> GL.Vertex2 a -> r) where
    drawPoint' esp mn cont attr value = drawPoint'' 2 esp mn cont attr [value]

-- Vector2 array
instance (PointType r, VertexAttribComponent a) => PointType (String -> [GL.Vertex2 a] -> r) where
    drawPoint' = drawPoint'' 2

drawPoint'' :: (PointType r, Storable a) =>
    NumComponents -> PointOp (PointOp (SketchMonad ()) -> String -> [a] -> r)
drawPoint'' compSize esp0 mn0 cont attr values = drawPoint' esp0 mn0 $ \esp1 mn1 -> do
    program <- lookupEsp esp1
    loc <- get $ attribLocation program attr
    vertexAttribArray loc GL.$= Enabled
--     io $ withArray values $ \ptr ->
    ptr <- io $ newArray values
    vertexAttribPointer loc GL.$=
      (ToFloat, VertexArrayDescriptor compSize Float 0 ptr) -- ToFloat XXX
    let nn = length values
    if mn1 == Just nn || isNothing mn1
        then do
            cont esp1 (Just nn)
            vertexAttribArray loc GL.$= Disabled
        else error "Inconsistent array compSizes when drawing points"
