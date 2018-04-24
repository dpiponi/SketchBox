-- Probably need to delete old program

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Sketch where

-- import Numeric.LinearAlgebra.HMatrix as H hiding (reshape)
-- import Data.Array as A
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

import System.FilePath

import GLCode
import SDLCode

data World = World {
        _windowSize :: (Int32, Int32),
        _shaderProgram :: M.Map String GL.Program,
        _mainWindow :: SDL.Window,
        _startTime :: UTCTime
    }

$(makeLenses ''World)

data Options = Options {
        _shaderDirectory :: FilePath
    } deriving Show

$(makeLenses ''Options)

type SketchMonad a = StateT World IO a

class PointType a where
    drawPoint' :: String -> SketchMonad () -> a

instance (a ~ ()) => PointType (SketchMonad a) where
    drawPoint' name = id

drawPoint :: PointType a => String -> Int -> a
drawPoint name n = drawPoint' name $ do
    programs <- use shaderProgram
    let Just program = M.lookup name programs
    GL.vertexProgramPointSize GL.$= GL.Enabled
    GL.pointSprite GL.$= Enabled
    GL.blend GL.$= GL.Enabled
    GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    io $ GL.drawArrays GL.Points 0 (fromIntegral n)

-- Float array
instance (PointType r) => PointType (String -> [Float] -> r) where
    drawPoint' name s attr values = drawPoint' name $ do
            programs <- use shaderProgram
            let Just program = M.lookup name programs
            loc <- get $ attribLocation program attr
            vertexAttribArray loc GL.$= Enabled
            io $ withArray values $ \ptr ->
                vertexAttribPointer loc GL.$=
                  (ToFloat, VertexArrayDescriptor 1 Float 0 ptr)
            s
            vertexAttribArray loc GL.$= Disabled

-- Uniform Float
instance (PointType r) => PointType (String -> Float -> r) where
    drawPoint' name s attr value = drawPoint' name $ do
            programs <- use shaderProgram
            io $ print programs
            let Just program = M.lookup name programs
            loc <- get $ uniformLocation program attr
            uniform loc GL.$= value
            s

-- Vector2 array
instance (PointType r) => PointType (String -> [GL.Vertex2 Float] -> r) where
    drawPoint' name s attr values = drawPoint' name $ do
            programs <- use shaderProgram
            let Just program = M.lookup name programs
            loc <- get $ attribLocation program attr
            vertexAttribArray loc GL.$= Enabled
            io $ withArray values $ \ptr ->
                vertexAttribPointer loc GL.$=
                  (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
            s
            vertexAttribArray loc GL.$= Disabled

-- Uniform vec2f
instance (PointType r) => PointType (String -> GL.Vertex2 Float -> r) where
    drawPoint' name s attr value = drawPoint' name $ do
            programs <- use shaderProgram
            let Just program = M.lookup name programs
            loc <- get $ uniformLocation program attr
            uniform loc GL.$= value

drawLine :: GL.Program -> GL.Vertex2 Float -> GL.Vertex2 Float -> SketchMonad ()
drawLine program p0 p1 = do
    let vertices = [p0, p1]
    let tvertices = [GL.Vertex2 0.0 1.0, GL.Vertex2 1.0 0.0] :: [GL.Vertex2 Float]
    positionLoc <- get $ attribLocation program "vPosition"
    texLoc <- get $ attribLocation program "texCoord"

    vertexAttribArray positionLoc GL.$= Enabled
    vertexAttribArray texLoc GL.$= Enabled
    io $ withArray vertices $ \ptr ->
        vertexAttribPointer positionLoc GL.$=
          (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
    io $ withArray tvertices $ \ptr ->
        vertexAttribPointer texLoc GL.$=
          (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
    io $ drawArrays Lines 0 2
    vertexAttribArray positionLoc GL.$= Disabled
    vertexAttribArray texLoc GL.$= Disabled

drawLine'' :: LineType a => GL.Program -> a
drawLine'' program = drawLine' program $ do
    blend GL.$= Enabled
    blendFunc GL.$= (SrcAlpha, OneMinusSrcAlpha)
    io $ drawArrays Lines 0 2
    io $ print "Drawing lines"

class LineType a where
    drawLine' :: GL.Program -> SketchMonad () -> a

instance (a ~ ()) => LineType (SketchMonad a) where
    drawLine' program = id

instance (LineType r) => LineType (String -> Float -> Float -> r) where
    drawLine' program s attr value0 value1 = drawLine' program $ do
            loc <- get $ attribLocation program attr
            vertexAttribArray loc GL.$= Enabled
            io $ withArray [value0, value1] $ \ptr ->
                vertexAttribPointer loc GL.$=
                  (ToFloat, VertexArrayDescriptor 1 Float 0 ptr)
            s
            vertexAttribArray loc GL.$= Disabled

instance (LineType r) => LineType (String -> GL.Vertex2 Float -> GL.Vertex2 Float -> r) where
    drawLine' program s attr value0 value1 = drawLine' program $ do
            loc <- get $ attribLocation program attr
            vertexAttribArray loc GL.$= Enabled
            io $ withArray [value0, value1] $ \ptr ->
                vertexAttribPointer loc GL.$=
                  (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
            s
            vertexAttribArray loc GL.$= Disabled

-- Some duplication here XXX
init :: FilePath -> IO World
init path = do
    window <- initWindow

    start <- getCurrentTime
    let world = World {
        _windowSize = (512, 512),
        _shaderProgram = M.empty,
        _mainWindow = window,
        _startTime = start
    }
    programs <- installShaders path
    return $ world & shaderProgram .~ M.fromList programs

withProgram :: String -> (GL.Program -> StateT World IO ()) -> StateT World IO ()
withProgram name cmd = do
    programs <- use shaderProgram
    case M.lookup name programs of
        Nothing -> return ()
        Just program -> cmd program

-- Going to be user responsibility to set window size
reshape :: (Int32, Int32) -> StateT World IO ()
reshape (w, h) = do
    -- withProgram "shader" $ \program -> io $ setShaderWindow program (w, h)
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (i w) (i h))
    windowSize .= (w, h)

-- Going to be user resposibility to set mouse
mouse :: (Int32, Int32) -> StateT World IO ()
mouse _ = return ()

-- mouse (x, y) = withProgram $ \program -> do
--     (_, h) <- use windowSize
--     io $ setShaderMouse program (x, h-y)

handleKey :: SDL.Keysym -> StateT World IO Bool
handleKey (SDL.Keysym {SDL.keysymScancode = SDL.ScancodeEscape}) = return True
handleKey (SDL.Keysym { }) = return False

handlePayload :: SDL.EventPayload -> StateT World IO Bool
handlePayload (SDL.WindowResizedEvent
                            (SDL.WindowResizedEventData { SDL.windowResizedEventSize = V2 w h })) =
                            reshape (w, h) >> return False
handlePayload (SDL.MouseMotionEvent
                            (SDL.MouseMotionEventData { SDL.mouseMotionEventPos = P (V2 x y) })) =
        mouse (x, y) >> return False
handlePayload (SDL.KeyboardEvent
                            (SDL.KeyboardEventData { SDL.keyboardEventKeyMotion = SDL.Pressed, SDL.keyboardEventKeysym = k })) =
        handleKey k
handlePayload SDL.QuitEvent = return True
handlePayload _ = return False

handleUIEvent :: SDL.Event -> StateT World IO Bool
handleUIEvent SDL.Event { SDL.eventPayload = payload} = handlePayload payload

parse :: Options -> [String] -> Options
parse options [] = options
parse options ("-d" : path : args) = parse (options { _shaderDirectory = path }) args
parse options args = error ("Incomprehensible options " ++ unwords args)

mainLoop :: (Float -> StateT World IO ()) -> IO ()
mainLoop render = do
    args <- getArgs
    let options = parse (Options { _shaderDirectory = "." }) args
    print options
     
    SDL.initialize [SDL.InitVideo]

    world <- init (_shaderDirectory options)

    evalStateT (loop options render) world

loop :: Options -> (Float -> StateT World IO ()) -> StateT World IO ()
loop options render = do
    window <- use mainWindow
    interval <- realToFrac <$> (diffUTCTime <$> io getCurrentTime <*> use startTime)
    -- user going to set time in shader
    -- withProgram $ \program -> io $ setShaderTime program interval
    render interval
    io $ SDL.glSwapWindow window
    events <- io SDL.pollEvents
    quit <- mapM handleUIEvent events
    unless (or quit) $ loop options render

writeGif :: Int -> Int -> ForeignPtr (PixelBaseComponent PixelRGB8) -> String -> IO ()
writeGif width height pixelData filename = do
    let array = SV.unsafeFromForeignPtr0 pixelData (width*height*3) :: SV.Vector (PixelBaseComponent PixelRGB8)
    let image = Image width height array :: Image PixelRGB8
    let (im, pa) = palettize defaultPaletteOptions image
    let Right zzz = writeGifImageWithPalette filename im pa
    io zzz

gifLoop :: Int -> Int -> Options -> (Float -> SketchMonad ()) -> SketchMonad ()
gifLoop i n _ _ | i >= n = return ()
gifLoop i n options render = do
    window <- use mainWindow
    let interval = fromIntegral i
    --user going to set time in shader
    --withProgram $ \program -> io $ setShaderTime program interval
    render interval
    pixelData <- io $ (mallocForeignPtrArray (512*512*3) :: IO (ForeignPtr (PixelBaseComponent PixelRGB8)))
    io $ writeGif 512 512 pixelData $ "xxx." ++ show (1500+i) ++ ".gif"
    io $ SDL.glSwapWindow window
    events <- io SDL.pollEvents
    quit <- mapM handleUIEvent events
    unless (or quit) $ gifLoop (i+1) n options render

initSketch :: IO ()
initSketch = do
    SDL.initialize [SDL.InitVideo]

mainGifLoop :: (Float -> SketchMonad ()) -> IO ()
mainGifLoop render = do
    initSketch
    args <- getArgs
    let options = parse (Options { _shaderDirectory = "." }) args
    print options
    world <- init (_shaderDirectory options)
    io $ evalStateT (gifLoop (-10) 210 options render) world
