{-# LANGUAGE TemplateHaskell #-}

module Engine.Engine (newApp, run, spr, sprEx, load, move, render, handleInput,
                RenderItem(), sprOptions, sprColor, tile, rot, line) where

import Data.IORef
import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT (($=))
import System.Exit
import Engine.Textures
import Data.HashTable
import Data.Maybe
import Debug.Trace
import Control.Monad

import Engine.Data
import Engine.Vbo
import Engine.RenderItems

newApp = App {  
        load = return id,
        move = id,
        render = \_ -> [],
        handleInput = \_ _ -> id
    }

run :: App appState -> IO()
run app = do
    (progname, _) <- GL.getArgsAndInitialize
    window <- initGL

    spriteImages <- getAndCreateTextures "data"
    sprites <- fromList hashString []
    mapM_ (\(name, tex) -> insert sprites name tex) spriteImages

    let resources = Resources {
            textures = sprites
        }
 
    initAppState <- load app

    engineStateRef <- newIORef $ EngineState { tick = 0, appState = initAppState, app, resources }
    
    GL.perWindowKeyRepeat $= GL.PerWindowKeyRepeatOff
    GL.displayCallback $= (display engineStateRef)
    --GL.closeCallback $= Just (closeHandler window)
    GL.idleCallback $= Just (idle engineStateRef)
    GL.keyboardMouseCallback $= Just (keyboardMouse window engineStateRef)
    GL.mainLoop
  
quit window = do 
    GL.destroyWindow window
    exitWith ExitSuccess 

-- Rendering
   
display engineStateRef = do 
    EngineState{tick, appState, app = (App {render}), resources} <- GL.get engineStateRef
    let renderItems = render appState

    -- GL.viewport $= (GL.Position 0 0, GL.Size 800 600)
    GL.clearColor $= GL.Color4 0.5 0.5 0.5 (0.0 :: Float)
    GL.clear [GL.ColorBuffer]
 
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho 0 800 600 0 (-1.0) (1.0)

    GL.blendEquation $= GL.FuncAdd 
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.blend $= GL.Enabled
 
    GL.textureFunction $= GL.Modulate
    GL.depthFunc $= Nothing
    GL.cullFace $= Nothing

    mapM_ (renderItem resources) renderItems

    GL.swapBuffers
    GL.flush


-- Move 

idle engineStateRef = do
    engineState <- GL.get engineStateRef
    let App{move} = app engineState
    time <- GL.get GL.elapsedTime
    engineStateRef $= moveEngine time engineState move
    GL.postRedisplay Nothing


stepPeriod = 6

moveEngine :: Int -> EngineState engineState -> (engineState -> engineState) -> EngineState engineState
moveEngine newTick state@EngineState{tick = 0} _ = state{tick=newTick}
moveEngine newTick state@EngineState{tick, appState} moveApp =
    let times = (newTick - tick) `div` stepPeriod 
        newAppState = iterate moveApp appState !! times
    in state{tick = (tick + stepPeriod * times), appState = newAppState}

initGL = do
    GL.initialDisplayMode $= [GL.DoubleBuffered, GL.RGBAMode]
    GL.initialWindowSize $= GL.Size 800 600
    window <- GL.createWindow "HPacman"
    return window


-- Input

closeHandler window = do
    GL.destroyWindow window
    exitWith ExitSuccess

keyboardMouse window _ (GL.Char '\ESC') GL.Down _ _ = do
    GL.destroyWindow window
    exitWith ExitSuccess

keyboardMouse _ engineStateRef key state modifiers position = do
    currentState <- GL.get engineStateRef
    let EngineState{appState, app = App{handleInput}} = currentState
    engineStateRef $= currentState{ appState = handleInput key state appState }