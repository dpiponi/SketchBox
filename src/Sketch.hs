{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Sketch where

import Codec.Picture.ColorQuant
import Codec.Picture.Gif
import Numeric.LinearAlgebra.HMatrix as H hiding (reshape)
import Codec.Picture.Types
import Data.Int
import Foreign
import System.Environment
import Data.Time
import qualified SDL
import Prelude hiding (init)
import Graphics.Rendering.OpenGL as GL
import SDL.Vect
import Control.Monad.State as S hiding (get)
import Control.Lens
import qualified Data.Vector.Storable as SV
import Data.Map.Strict as M

import GLCode
import SDLCode

data World = World {
        _windowSize :: (Int32, Int32),
        _shaderProgram :: M.Map String GL.Program,
        _mainWindow :: SDL.Window,
        _startTime :: UTCTime
    }

$(makeLenses ''World)

newtype Options = Options {
        _shaderDirectory :: FilePath
    } deriving Show

$(makeLenses ''Options)

type SketchMonad a = StateT World IO a

lookupProgram :: String -> SketchMonad Program
lookupProgram name = do
    programs <- use shaderProgram
    let Just program = M.lookup name programs
    return program

lookupEsp :: Either String Program -> SketchMonad Program
lookupEsp (Left name) = lookupProgram name
lookupEsp (Right program) = return program

-- Uniforms

class UniformType a where
    setUniform' :: Either String Program ->
                  (Either String Program -> SketchMonad ()) -> a

instance (a ~ ()) => UniformType (SketchMonad a) where
    setUniform' program cont = cont program

setUniform :: UniformType a => String -> a
setUniform name = setUniform' (Left name) $ \_ -> return ()

-- Uniform Float
instance (UniformType r) => UniformType (String -> Float -> r) where
    setUniform' esp0 cont attr value = setUniform' esp0 $ \esp1 -> do
        program <- lookupEsp esp1
        currentProgram $= Just program
        loc <- get $ uniformLocation program attr
        uniform loc GL.$= value
        cont esp1

-- Uniform vec2f
instance (UniformType r) => UniformType (String -> GL.Vertex2 Float -> r) where
    setUniform' esp0 cont attr value = setUniform' esp0 $ \esp1 -> do
        program <- lookupEsp esp1
        currentProgram $= Just program
        loc <- get $ uniformLocation program attr
        uniform loc GL.$= value
        cont esp1

-- Uniform mat4f
instance (UniformType r) => UniformType (String -> GL.GLmatrix Float -> r) where
    setUniform' esp0 cont attr value = setUniform' esp0 $ \esp1 -> do
        program <- lookupEsp esp1
        currentProgram $= Just program
        loc <- get $ uniformLocation program attr
        uniform loc GL.$= value
        cont esp1

-- Uniform mat4f
instance (UniformType r) => UniformType (String -> H.Matrix Float -> r) where
    setUniform' esp0 cont attr value = setUniform' esp0 $ \esp1 -> do
        program <- lookupEsp esp1
        currentProgram $= Just program
        loc <- get $ uniformLocation program attr
        matr <- io $ GL.newMatrix @GL.GLmatrix @Float GL.ColumnMajor $ concat $ H.toLists $ value
        uniform loc GL.$= matr
        cont esp1

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
    forM_ (M.lookup name programs) cmd

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
handleKey SDL.Keysym {SDL.keysymScancode = SDL.ScancodeEscape} = return True
handleKey SDL.Keysym { } = return False

handlePayload :: SDL.EventPayload -> StateT World IO Bool
handlePayload (SDL.WindowResizedEvent
                            SDL.WindowResizedEventData { SDL.windowResizedEventSize = V2 w h }) =
                            reshape (w, h) >> return False
handlePayload (SDL.MouseMotionEvent
                            SDL.MouseMotionEventData { SDL.mouseMotionEventPos = P (V2 x y) }) =
        mouse (x, y) >> return False
handlePayload (SDL.KeyboardEvent
                            SDL.KeyboardEventData { SDL.keyboardEventKeyMotion = SDL.Pressed, SDL.keyboardEventKeysym = k }) =
        handleKey k
handlePayload SDL.QuitEvent = return True
handlePayload _ = return False

handleUIEvent :: SDL.Event -> StateT World IO Bool
handleUIEvent SDL.Event { SDL.eventPayload = payload} = handlePayload payload

parse :: Options -> [String] -> Options
parse options [] = options
parse options ("-d" : path : args) = parse (options { _shaderDirectory = path }) args
parse _ args = error ("Incomprehensible options " ++ unwords args)

{-
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
-}

makeGif :: Int32 -> Int32 -> ForeignPtr (PixelBaseComponent PixelRGB8) -> Image PixelRGB8 
makeGif width32 height32 pixelData =
    let width = fromIntegral width32
        height = fromIntegral height32
        array = SV.unsafeFromForeignPtr0 pixelData (width*height*3) :: SV.Vector (PixelBaseComponent PixelRGB8)
    in Image width height array

gifLoop :: String -> Float -> Int -> Int -> Options -> [(Palette, GifDelay, Image Pixel8)] -> (Float -> SketchMonad ()) -> SketchMonad ()
gifLoop filename _ j n _ frames _ | j >= n = do
    let Right zzz = writeGifImages filename LoopingForever frames 
    io zzz
gifLoop filename fps j n options frames render = do
    window <- use mainWindow
    let interval = fromIntegral j
    render (interval/fps)
    (width, height) <- use windowSize
    pixelData <- io (mallocForeignPtrArray (fromIntegral $ width*height*3) :: IO (ForeignPtr (PixelBaseComponent PixelRGB8)))
    (im, pa) <- io $ withForeignPtr pixelData $ \ptr -> do
                GL.readPixels (GL.Position 0 0) (GL.Size width height) $ GL.PixelData GL.RGB GL.UnsignedByte ptr
                let gif = makeGif width height pixelData
                let (im, pa) = palettize defaultPaletteOptions gif
                return (im, pa)
    io $ SDL.glSwapWindow window
    events <- io SDL.pollEvents
    quit <- mapM handleUIEvent events
    unless (or quit) $ gifLoop filename fps (j+1) n options ((pa, 4, im) : frames) render

initSketch :: IO Options
initSketch = do
    SDL.initialize [SDL.InitVideo]
    parse Options { _shaderDirectory = "." } <$> getArgs

mainGifLoop :: String -> Float -> Int -> Int ->(Float -> SketchMonad ()) -> IO ()
mainGifLoop filename fps start end render = do
    options <- initSketch
    world <- init (_shaderDirectory options)
    io $ evalStateT (gifLoop filename fps start end options [] render) world

loop :: Options -> (Float -> SketchMonad ()) -> SketchMonad ()
loop options render = do
    window <- use mainWindow
    interval <- realToFrac <$> (diffUTCTime <$> io getCurrentTime <*> use startTime)
    render interval
    io $ SDL.glSwapWindow window
    events <- io SDL.pollEvents
    quit <- mapM handleUIEvent events
    unless (or quit) $ loop options render

mainLoop :: (Float -> SketchMonad ()) -> IO ()
mainLoop render = do
    options <- initSketch
    world <- init (_shaderDirectory options)
    io $ evalStateT (loop options render) world
