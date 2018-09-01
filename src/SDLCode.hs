{-# LANGUAGE OverloadedStrings #-}

module SDLCode where

import SDL

import qualified Graphics.Rendering.OpenGL as GL
import Foreign.C.Types

openGLConfig :: CInt -> OpenGLConfig
openGLConfig samples = OpenGLConfig {
        glColorPrecision = V4 8 8 8 0,
        glDepthPrecision = 24,
        glStencilPrecision = 8,
        glMultisampleSamples = samples,
        glProfile = Compatibility Normal 2 1
    }

-- initWindowSize :: V2 CInt
-- initWindowSize = V2 512 512

initWindow :: CInt -> CInt -> CInt -> IO Window
initWindow width height samples = do
    window <- createWindow "LitterBox"
                defaultWindow {
                    windowInitialSize = V2 width height,
                    windowResizable = True,
                    windowOpenGL = Just (openGLConfig samples)
                }

    showWindow window
    _ <- glCreateContext window
    swapInterval GL.$= SynchronizedUpdates
    return window
