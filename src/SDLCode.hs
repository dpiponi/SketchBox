{-# LANGUAGE OverloadedStrings #-}

module SDLCode where

import SDL

import qualified Graphics.Rendering.OpenGL as GL
import Foreign.C.Types

openGLConfig :: OpenGLConfig
openGLConfig = OpenGLConfig {
        glColorPrecision = V4 8 8 8 0,
        glDepthPrecision = 24,
        glStencilPrecision = 8,
        glMultisampleSamples = 1,
        glProfile = Compatibility Normal 2 1
    }

initWindowSize :: V2 CInt
initWindowSize = V2 512 512

initWindow :: IO Window
initWindow = do
    window <- createWindow "LitterBox"
                defaultWindow {
                    windowInitialSize = initWindowSize,
                    windowResizable = True,
                    windowOpenGL = Just openGLConfig
                }

    showWindow window
    _ <- glCreateContext window
    swapInterval GL.$= SynchronizedUpdates
    return window
