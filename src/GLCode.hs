{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module GLCode where

import Control.Monad.Except
import Data.List
import System.Directory
import Graphics.Rendering.OpenGL
import Foreign.Marshal.Array
import qualified Data.ByteString as B
import Control.Exception
import System.FilePath
import Data.Int

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
liftCatch f m = ExceptT $ fmap (either (Left . f) Right) (try m)

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

installShaders :: FilePath -> IO [(String, Program)]
installShaders path = do
    files <- liftIO $ getDirectoryContents path
    let files' = nub $ map dropExtension files
    io $ print files'
    fs <- forM files' $ \filename -> do
        let vertName = addExtension filename ".vert"
        let vertPath = joinPath [path, vertName]
        let fragName = addExtension filename ".frag"
        let fragPath = joinPath [path, fragName]
        vertExists <- liftIO $ doesFileExist vertPath
        fragExists <- liftIO $ doesFileExist fragPath
        putStrLn $ "Looking for " ++ vertName ++ ", " ++ fragName
        if vertExists && fragExists
            then do
                io $ putStrLn $ "Installing '" ++ filename ++ "'"
                eprogram <- runExceptT $ loadShaders [
                    ShaderInfo VertexShader vertPath,
                    ShaderInfo FragmentShader fragPath]
                case eprogram of
                    Left e -> do
                        io $ putStrLn $ "Failed " ++ filename ++ ": " ++ e
                        return []
                    Right program -> do
                        io $ putStrLn $ "Compiled " ++ filename
                        currentProgram $= Just program
                        io $ setShaderWindow program (512, 512)
                        return [(filename, program)]
            else return []
    return $ concat fs

compileProgram :: FilePath -> String -> IO Program
compileProgram path name = do
    let vertexPath = joinPath [path, name ++ ".vert"]
    let fragmentPath = joinPath [path, name ++ ".frag"]
    print vertexPath
    print fragmentPath
    Right program <- runExceptT $ loadShaders [
        ShaderInfo VertexShader vertexPath,
        ShaderInfo FragmentShader fragmentPath]

    currentProgram $= Just program
    setShaderWindow program (512, 512)

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
    let verts = [p0, p1, p2]
    vertexAttribArray (AttribLocation 0) $= Enabled
    withArray verts $ \ptr ->
        vertexAttribPointer (AttribLocation 0) $=
          (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
    drawArrays Triangles 0 3
    vertexAttribArray (AttribLocation 0) $= Disabled

-- http://snak.tdiary.net/20100209.html

{-
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
-}
--     print "Drawing points"
