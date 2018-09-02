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

-- Vector3 array
instance (TriangleType r, VertexAttribComponent a) => TriangleType (String -> [GL.Vertex3 a] -> r) where
    drawTriangle' = drawTriangle'' 3

-- Vector4 array
instance (TriangleType r, VertexAttribComponent a) => TriangleType (String -> [GL.Vertex4 a] -> r) where
    drawTriangle' = drawTriangle'' 4

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

drawTriangle'' :: (TriangleType r, Storable a) =>
    NumComponents -> TriangleOp (TriangleOp (SketchMonad ()) -> String -> [a] -> r)
drawTriangle'' size esp0 mn0 cont attr values = drawTriangle' esp0 mn0 $ \esp1 mn1 -> do
    program <- lookupEsp esp1
    loc <- get $ attribLocation program attr
    vertexAttribArray loc GL.$= Enabled
    arrayBuffer <- GL.genObjectName
    io $ print arrayBuffer
    bindBuffer ArrayBuffer $= Just arrayBuffer
    io $ withArray values $ \ptr -> do
              let size = fromIntegral (length values * sizeOf (head values))
              bufferData ArrayBuffer $= (size, ptr, StaticDraw)
--     io $ withArray values $ \ptr ->
--         vertexAttribPointer loc GL.$=
--           (ToFloat, VertexArrayDescriptor size Float 0 ptr) -- ToFloat XXX
    vertexAttribPointer loc $=
            (ToFloat, VertexArrayDescriptor size Float 0 (bufferOffset 0))
    let nn = length values
    if mn1 == Just nn || isNothing mn1
        then do
            cont esp1 (Just nn)
            vertexAttribArray loc GL.$= Disabled
        else error "Inconsistent array sizes when drawing lines"
