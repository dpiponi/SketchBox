{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module GLCode where

import Control.Monad.Except
import Graphics.Rendering.OpenGL
import Foreign.Marshal.Array
import qualified Data.ByteString as B
import Control.Exception
import System.FilePath
import Data.Int
import Control.Monad.Writer

io :: MonadIO m => IO a -> m a
io = liftIO

voidIO :: MonadIO m => IO () -> m ()
voidIO = liftIO

i :: (Num b, Integral a) => a -> b
i = fromIntegral

data ShaderInfo = ShaderInfo ShaderType FilePath
    deriving (Eq, Ord, Show)

compileAndCheck :: Shader -> ExceptT String IO ()
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

liftCatch :: (IOException -> e) -> IO a -> ExceptT e IO a
liftCatch f m = ExceptT $ liftM (either (Left . f) Right) (try m)

loadCompileAttach :: Program -> [ShaderInfo] -> ExceptT String IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source : infos) = ExceptT $
   createShader shType `bracketOnError` deleteObjectName $ \shader -> runExceptT $ do
      src <- liftCatch show (B.readFile source)
      shaderSourceBS shader $= src
      compileAndCheck shader
      io $ attachShader program shader
      loadCompileAttach program infos

checked :: (t -> IO ())
        -> (t -> GettableStateVar Bool)
        -> (t -> GettableStateVar String)
        -> String
        -> t
        -> ExceptT String IO ()
checked action getStatus getInfoLog message object = do
    io $ action object
    ok <- get (getStatus object)
    unless ok $ do
        infoLog <- get (getInfoLog object)
        throwError (message ++ " log: " ++ infoLog)

linkAndCheck :: Program -> ExceptT String IO ()
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

loadShaders :: [ShaderInfo] -> ExceptT String IO Program
loadShaders infos = ExceptT $
   createProgram `bracketOnError` deleteObjectName $ \program -> runExceptT $ do
      loadCompileAttach program infos
      linkAndCheck program
      return program

installShaders :: FilePath -> ExceptT String IO Program
installShaders path = do
    program <- loadShaders [
        ShaderInfo VertexShader (joinPath [path, "litterbox.vert"]),
        ShaderInfo FragmentShader (joinPath [path, "litterbox.frag"])]

    currentProgram $= Just program
    io $ setShaderWindow program (512, 512)

    return program

compileProgram :: FilePath -> String -> ExceptT String IO Program
compileProgram path name = do
    program <- loadShaders [
        ShaderInfo VertexShader (joinPath [path, name ++ ".vert"]),
        ShaderInfo FragmentShader (joinPath [path, name ++ ".frag"])]

    currentProgram $= Just program
    io $ setShaderWindow program (512, 512)

    return program

vertices :: [Vertex2 GLfloat]
vertices = [
    Vertex2 (-1.00) (-1.00),  -- Triangle 1
    Vertex2   1.00  (-1.00),
    Vertex2 (-1.00)   1.00,
    Vertex2   1.00  (-1.00),  -- Triangle 2
    Vertex2   1.00    1.00,
    Vertex2 (-1.00)   1.00]

setShaderTime :: Program -> Float -> IO ()
setShaderTime program time = do
    loc <- uniformLocation program "iTime"
    uniform loc $= time

setShaderWindow :: Program -> (Int32, Int32) -> IO ()
setShaderWindow program (w, h) = do
    loc <- uniformLocation program "iResolution"
    uniform loc $= Vector2 (i w) (i h::Float)

setShaderMouse :: Program -> (Int32, Int32) -> IO ()
setShaderMouse program (w, h) = do
    loc <- uniformLocation program "iMouse"
    uniform loc $= Vector2 (i w) (i h::Float)

drawTriangle :: Vertex2 Float -> Vertex2 Float -> Vertex2 Float -> IO ()
drawTriangle p0 p1 p2 = do
    let vertices = [p0, p1, p2]
    vertexAttribArray (AttribLocation 0) $= Enabled
    withArray vertices $ \ptr ->
        vertexAttribPointer (AttribLocation 0) $=
          (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
    drawArrays Triangles 0 3
    vertexAttribArray (AttribLocation 0) $= Disabled

-- http://snak.tdiary.net/20100209.html

drawPoints :: Program -> [Vertex2 Float] -> [(String, [Float])] -> IO ()
drawPoints program vertices as = do
    positionLoc <- get $ attribLocation program "vPosition"

    vertexAttribArray positionLoc $= Enabled
    withArray vertices $ \ptr ->
        vertexAttribPointer positionLoc $=
          (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)

    forM_ as $ \(attr, values) -> do
        loc <- get $ attribLocation program attr
        vertexAttribArray loc $= Enabled
        withArray values $ \ptr ->
            vertexAttribPointer loc $=
              (ToFloat, VertexArrayDescriptor 1 Float 0 ptr)

drawPoint :: PointType a => Program -> Int -> a
drawPoint program n = drawPoint' program $ do
    vertexProgramPointSize $= Enabled
    pointSprite $= Enabled
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    drawArrays Points 0 (fromIntegral n)
--     print "Drawing points"

class PointType a where
    drawPoint' :: Program -> IO () -> a

instance (a ~ ()) => PointType (IO a) where
    drawPoint' program = id

-- Float array
instance (PointType r) => PointType (String -> [Float] -> r) where
    drawPoint' program s attr values = drawPoint' program $ do
            loc <- get $ attribLocation program attr
            vertexAttribArray loc $= Enabled
            withArray values $ \ptr ->
                vertexAttribPointer loc $=
                  (ToFloat, VertexArrayDescriptor 1 Float 0 ptr)
            s
            vertexAttribArray loc $= Disabled

-- Uniform Float
instance (PointType r) => PointType (String -> Float -> r) where
    drawPoint' program s attr value = drawPoint' program $ do
            loc <- get $ uniformLocation program attr
            uniform loc $= value
            s

-- Vector2 array
instance (PointType r) => PointType (String -> [Vertex2 Float] -> r) where
    drawPoint' program s attr values = drawPoint' program $ do
            loc <- get $ attribLocation program attr
            vertexAttribArray loc $= Enabled
            withArray values $ \ptr ->
                vertexAttribPointer loc $=
                  (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
            s
            vertexAttribArray loc $= Disabled

-- Uniform vec2f
instance (PointType r) => PointType (String -> Vertex2 Float -> r) where
    drawPoint' program s attr value = drawPoint' program $ do
            loc <- get $ uniformLocation program attr
            uniform loc $= value

drawLine :: Program -> Vertex2 Float -> Vertex2 Float -> IO ()
drawLine program p0 p1 = do
    let vertices = [p0, p1]
    let tvertices = [Vertex2 0.0 1.0, Vertex2 1.0 0.0] :: [Vertex2 Float]
    positionLoc <- get $ attribLocation program "vPosition"
    texLoc <- get $ attribLocation program "texCoord"

    vertexAttribArray positionLoc $= Enabled
    vertexAttribArray texLoc $= Enabled
    withArray vertices $ \ptr ->
        vertexAttribPointer positionLoc $=
          (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
    withArray tvertices $ \ptr ->
        vertexAttribPointer texLoc $=
          (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
    drawArrays Lines 0 2
    vertexAttribArray positionLoc $= Disabled
    vertexAttribArray texLoc $= Disabled

drawLine'' :: LineType a => Program -> a
drawLine'' program = drawLine' program $ do
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    drawArrays Lines 0 2
    print "Drawing lines"

class LineType a where
    drawLine' :: Program -> IO () -> a

instance (a ~ ()) => LineType (IO a) where
    drawLine' program = id

instance (LineType r) => LineType (String -> Float -> Float -> r) where
    drawLine' program s attr value0 value1 = drawLine' program $ do
            loc <- get $ attribLocation program attr
            vertexAttribArray loc $= Enabled
            withArray [value0, value1] $ \ptr ->
                vertexAttribPointer loc $=
                  (ToFloat, VertexArrayDescriptor 1 Float 0 ptr)
            s
            vertexAttribArray loc $= Disabled

instance (LineType r) => LineType (String -> Vertex2 Float -> Vertex2 Float -> r) where
    drawLine' program s attr value0 value1 = drawLine' program $ do
            loc <- get $ attribLocation program attr
            vertexAttribArray loc $= Enabled
            withArray [value0, value1] $ \ptr ->
                vertexAttribPointer loc $=
                  (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
            s
            vertexAttribArray loc $= Disabled
