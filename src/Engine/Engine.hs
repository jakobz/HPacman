{-# LANGUAGE TemplateHaskell #-}

module Engine.Engine (newApp, run, spr, sprEx, load, move, render, handleInput,
                RenderItem(), sprOptions, sprColor, tile, rot, line, prepareBatch) where

import Data.IORef
import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT (($=))
import System.Exit
import Engine.Textures
import Data.Map
import Debug.Trace
import System.FSNotify
import qualified Filesystem.Path.CurrentOS as Path

import Engine.Data
import Engine.FileUtils
import Engine.RenderItems
import Engine.Shaders


newApp = App {  
        load = return id,
        move = id,
        render = \_ -> [],
        handleInput = \_ _ -> id
    }

run :: App appState -> IO()
run app = do
    (_, _) <- GL.getArgsAndInitialize
    window <- initGL

    spriteImages <- getAndCreateTextures "images"
    let sprites = fromList spriteImages

    shader <- newProgram "default"

    let resources = Resources {
            textures = sprites,
            shaders = fromList [("default", shader)]
        }
 
    initAppState <- load app

    engineStateRef <- newIORef $ EngineState { tick = 0, appState = initAppState, app, resources }

    let acceptFSEvent (Modified _ _) = True
        acceptFSEvent _ = False

    withManager $ \manager -> do
        let dir = Path.decodeString "."
        putStrLn $ "Watching " ++ show dir
        watchTree manager dir acceptFSEvent (\e -> print e)

        GL.perWindowKeyRepeat $= GL.PerWindowKeyRepeatOff
        GL.displayCallback $= (display engineStateRef)
        GL.closeCallback $= Just (closeHandler window)
        GL.idleCallback $= Just (idle engineStateRef)
        GL.keyboardMouseCallback $= Just (keyboardMouse window engineStateRef)
        GL.mainLoop
  
-- Rendering

display :: IORef (EngineState a) -> IO ()
display engineStateRef = do 
    EngineState{tick, appState, app = (App {render}), resources} <- GL.get engineStateRef
    let renderItems = render appState

    -- GL.viewport $= (GL.Position 0 0, GL.Size 800 600)
    GL.clearColor $= GL.Color4 0.5 0.5 0.5 (0.0 :: GL.GLfloat)
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

modifyIORef' ref f = do  -- exists in newer versions of base 
     x <- readIORef ref 
     let x' = f x 
     x' `seq` writeIORef ref x' 

idle engineStateRef = do
    engineState <- GL.get engineStateRef
    let App{move} = app engineState
    time <- GL.get GL.elapsedTime
    modifyIORef' engineStateRef $ moveEngine time move -- strick application required to avoid stack overflow
    GL.postRedisplay Nothing


stepPeriod = 6

moveEngine :: Int -> (engineState -> engineState) -> EngineState engineState -> EngineState engineState
moveEngine newTick _ state@EngineState{tick = 0} = state{tick=newTick}
moveEngine newTick moveApp state@EngineState{tick, appState} =
    let times = (newTick - tick) `div` stepPeriod 
        newAppState = iterate moveApp appState !! times
        newAppTick = (tick + stepPeriod * times)
        newEngineState = state{tick = newAppTick, appState = newAppState} 
    in newEngineState

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
