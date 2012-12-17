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
import Engine.VBO
import Engine.Sprites

data App state = App {
        load :: IO state,
        move :: state -> state,
        render :: state -> [RenderItem],
        handleInput ::  GL.Key -> GL.KeyState -> state -> state
    }

data EngineState appState = 
    EngineState {
        tick :: Int,
        appState :: appState,
        app :: App appState,
        resources :: Resources
    } 

data Resources = Resources {
    textures :: HashTable String Tex
} 

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
    GL.clearColor $= GL.Color4 1.0 1.0 1.0 (0.0 :: Float)
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

renderItem :: Resources -> RenderItem -> IO()
renderItem Resources{textures} sprite@(SpriteInstance name _ _ _) = do
        maybeTexture <- Data.HashTable.lookup textures name  
        case maybeTexture of
            Just texture@(Tex textureID texW texH) -> do
                let ((c1,c2,c3,c4), (t1,t2,t3,t4), (r,g,b)) =
                        spriteToCoords (texW, texH) sprite
                GL.textureBinding GL.Texture2D $= Just textureID
                GL.texture GL.Texture2D $= GL.Enabled
                GL.color $ GL.Color4 r g b 1
                GL.renderPrimitive GL.Quads $ do
                    GL.texCoord t1
                    GL.vertex   c1
                    GL.texCoord t2
                    GL.vertex   c2
                    GL.texCoord t3
                    GL.vertex   c3
                    GL.texCoord t4
                    GL.vertex   c4
            Nothing -> error $ "Can't find texture " ++ name

renderItem textures line@(LineInstance points (r,g,b)) =
    do GL.color $ GL.Color4 r g b 1
       GL.texture GL.Texture2D $= GL.Disabled
       GL.renderPrimitive GL.LineStrip $ do
            forM_ points (\(x,y) -> GL.vertex $ GL.Vertex2 x y)


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

keyboardMouse window _ (GL.Char '\ESC') GL.Down _ _ = do
    GL.destroyWindow window
    exitWith ExitSuccess

keyboardMouse _ engineStateRef key state modifiers position = do
    currentState <- GL.get engineStateRef
    let EngineState{appState, app = App{handleInput}} = currentState
    engineStateRef $= currentState{ appState = handleInput key state appState }