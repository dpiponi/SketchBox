{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Triangles where

import Foreign
import Graphics.Rendering.OpenGL as GL
import Data.Maybe
import Sketch
import GLCode

type TriangleOp a = Either String Program -> Maybe Int -> a

class TriangleType a where
    drawTriangle' :: TriangleOp (TriangleOp (SketchMonad ()) -> a)

instance (a ~ ()) => TriangleType (SketchMonad a) where
    drawTriangle' program mn cont = cont program mn

drawTriangle :: TriangleType a => String -> a
drawTriangle name = drawTriangle' (Left name) Nothing $ \esp mn -> do
    program <- lookupEsp esp
    GL.blend GL.$= GL.Enabled
    GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    currentProgram $= Just program
    case mn of
        Just n -> io $ GL.drawArrays GL.Triangles 0 (fromIntegral n)
        Nothing -> error "Don't know array size when drawing triangles"

-- Float array
instance (TriangleType r) => TriangleType (String -> [Float] -> r) where
    drawTriangle' = drawTriangle'' 1

-- Vector2 array
instance (TriangleType r, VertexAttribComponent a) => TriangleType (String -> [GL.Vertex2 a] -> r) where
    drawTriangle' = drawTriangle'' 2

drawTriangle'' :: (TriangleType r, Storable a) =>
    NumComponents -> TriangleOp (TriangleOp (SketchMonad ()) -> String -> [a] -> r)
drawTriangle'' size esp0 mn0 cont attr values = drawTriangle' esp0 mn0 $ \esp1 mn1 -> do
    program <- lookupEsp esp1
    loc <- get $ attribLocation program attr
    vertexAttribArray loc GL.$= Enabled
    io $ withArray values $ \ptr ->
        vertexAttribPointer loc GL.$=
          (ToFloat, VertexArrayDescriptor size Float 0 ptr) -- ToFloat XXX
    let nn = length values
    if mn1 == Just nn || isNothing mn1
        then do
            cont esp1 (Just nn)
            vertexAttribArray loc GL.$= Disabled
        else error "Inconsistent array sizes when drawing lines"
