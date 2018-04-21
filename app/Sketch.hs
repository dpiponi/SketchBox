-- Probably need to delete old program

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Graphics.Rendering.OpenGL as GL
import SDL.Vect
import Control.Monad.Except
import Control.Monad.State as S
import Control.Lens
import qualified Data.Vector.Storable as SV

import System.FilePath

import GLCode
import SDLCode

data World a = World {
        _windowSize :: (Int32, Int32),
        _shaderProgram :: Maybe GL.Program,
        _mainWindow :: SDL.Window,
        _startTime :: UTCTime,
        _user :: a
    }

$(makeLenses ''World)

data Options = Options {
        _shaderDirectory :: FilePath
    } deriving Show

$(makeLenses ''Options)

-- Some duplication here XXX
init :: FilePath -> u -> IO (World u)
init path userData = do
    window <- initWindow

    start <- getCurrentTime
    let world = World (512, 512) Nothing window start userData
    runExceptT (installShaders path) >>= \case
        Left e -> putStrLn ("Error: " ++ e) >> return world
        Right program -> return $ world & shaderProgram .~ Just program

withProgram :: (GL.Program -> StateT (World u) IO ()) -> StateT (World u) IO ()
withProgram cmd = 
    use shaderProgram >>= \case
        Nothing -> return ()
        Just program -> cmd program

reshape :: (Int32, Int32) -> StateT (World u) IO ()
reshape (w, h) = do
    withProgram $ \program -> io $ setShaderWindow program (w, h)
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (i w) (i h))
    windowSize .= (w, h)

mouse :: (Int32, Int32) -> StateT (World u) IO ()
mouse (x, y) = withProgram $ \program -> do
    (_, h) <- use windowSize
    io $ setShaderMouse program (x, h-y)

handleKey :: SDL.Keysym -> StateT (World u) IO Bool
handleKey (SDL.Keysym {SDL.keysymScancode = SDL.ScancodeEscape}) = return True
handleKey (SDL.Keysym { }) = return False

handlePayload :: SDL.EventPayload -> StateT (World u) IO Bool
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

handleUIEvent :: SDL.Event -> StateT (World u) IO Bool
handleUIEvent SDL.Event { SDL.eventPayload = payload} = handlePayload payload

parse :: Options -> [String] -> Options
parse options [] = options
parse options ("-d" : path : args) = parse (options { _shaderDirectory = path }) args
parse options args = error ("Incomprehensible options " ++ unwords args)

mainLoop :: u -> (Float -> StateT (World u) IO ()) -> IO ()
mainLoop userData render = do
    args <- getArgs
    let options = parse (Options { _shaderDirectory = "." }) args
    print options
     
    SDL.initialize [SDL.InitVideo]

    world <- init (_shaderDirectory options) userData

    evalStateT (loop options render) world

loop :: Options -> (Float -> StateT (World u) IO ()) -> StateT (World u) IO ()
loop options render = do
    window <- use mainWindow
    interval <- realToFrac <$> (diffUTCTime <$> io getCurrentTime <*> use startTime)
    withProgram $ \program -> io $ setShaderTime program interval
    render interval
    io $ SDL.glSwapWindow window
    events <- io SDL.pollEvents
    quit <- mapM handleUIEvent events
    unless (or quit) $ loop options render

gifLoop :: Int -> Int -> Options -> (Float -> StateT (World u) IO ()) -> StateT (World u) IO ()
gifLoop i n _ _ | i >= n = return ()
gifLoop i n options render = do
    window <- use mainWindow
    let interval = fromIntegral i
    withProgram $ \program -> io $ setShaderTime program interval
    render interval
    pixelData <- io $ (mallocForeignPtrArray (512*512*3) :: IO (ForeignPtr (PixelBaseComponent PixelRGB8)))
    array <- io $ withForeignPtr pixelData $ \ptr -> do
                  io $ GL.readPixels (GL.Position 0 0) (GL.Size 512 512) $ GL.PixelData GL.RGB GL.UnsignedByte ptr
    let array = SV.unsafeFromForeignPtr0 pixelData (512*512*3) :: SV.Vector (PixelBaseComponent PixelRGB8)
    let image = Image 512 512 array :: Image PixelRGB8
    let (im, pa) = palettize defaultPaletteOptions image
    let filename = "xxx." ++ show (1500+i) ++ ".gif"
    let Right zzz = writeGifImageWithPalette filename im pa
    io zzz
    io $ SDL.glSwapWindow window
    events <- io SDL.pollEvents
    quit <- mapM handleUIEvent events
    unless (or quit) $ gifLoop (i+1) n options render

initSketch :: IO ()
initSketch = do
    SDL.initialize [SDL.InitVideo]

mainGifLoop :: u -> (Float -> StateT (World u) IO ()) -> IO ()
mainGifLoop userData render = do
    args <- getArgs
    let options = parse (Options { _shaderDirectory = "." }) args
    print options
    world <- init (_shaderDirectory options) userData
    evalStateT (gifLoop (-10) 210 options render) world
