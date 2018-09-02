{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Lines where

import Foreign
import Graphics.Rendering.OpenGL as GL
import Data.Maybe
import Sketch
import GLCode

type LineOp a = Either String Program -> Maybe Int -> a

class LineType a where
    drawLine' :: LineOp (LineOp (SketchMonad ()) -> a)

instance (a ~ ()) => LineType (SketchMonad a) where
    drawLine' program mn cont = cont program mn

drawLine :: LineType a => String -> a
drawLine name = drawLine' (Left name) Nothing $ \esp mn -> do
    program <- lookupEsp esp
    GL.blend GL.$= GL.Enabled
    GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    currentProgram $= Just program
    case mn of
        Just n -> do
            io $ GL.drawArrays GL.LineStrip 0 (fromIntegral n)
--             io GL.finish
        Nothing -> error "Don't know array size when drawing lines"

-- Float array
instance (LineType r) => LineType (String -> [Float] -> r) where
    drawLine' = drawLine'' 1

-- Vector2 array
instance (LineType r, VertexAttribComponent a) => LineType (String -> [GL.Vertex2 a] -> r) where
    drawLine' = drawLine'' 2

-- Vector3 array
instance (LineType r, VertexAttribComponent a) => LineType (String -> [GL.Vertex3 a] -> r) where
    drawLine' = drawLine'' 3

-- Vector4 array
instance (LineType r, VertexAttribComponent a) => LineType (String -> [GL.Vertex4 a] -> r) where
    drawLine' = drawLine'' 4

drawLine'' :: (LineType r, Storable a) =>
    NumComponents -> LineOp (LineOp (SketchMonad ()) -> String -> [a] -> r)
drawLine'' size esp0 mn0 cont attr values = drawLine' esp0 mn0 $ \esp1 mn1 -> do
    program <- lookupEsp esp1
    loc <- get $ attribLocation program attr
    vertexAttribArray loc GL.$= Enabled
    ptr <- io $ newArray values
--     io $ withArray values $ \ptr ->
    vertexAttribPointer loc GL.$=
      (ToFloat, VertexArrayDescriptor size Float 0 ptr) -- ToFloat XXX
    let nn = length values
    if mn1 == Just nn || isNothing mn1
        then do
            cont esp1 (Just nn)
            vertexAttribArray loc GL.$= Disabled
        else error "Inconsistent array sizes when drawing lines"
