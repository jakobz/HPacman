module Engine (newGame, run, spr, sprEx, load, move, render, handleInput, sprOptions, color) where

{-# OPTIONS -fglasgow-exts #-}
import Data.IORef
import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT (($=))
import System.Exit
import Textures
import Data.HashTable
import Data.Maybe
import Debug.Trace

data Game state = Game {
        load :: IO state,
        move :: state -> state,
        render :: state -> [SpriteInstance],
        handleInput ::  GL.Key -> GL.KeyState -> state -> state
    }

newGame = Game {  
        load = return id,
        move = id,
        render = \_ -> [],
        handleInput = \_ _ -> id
    }

data SpriteOptions = SpriteOptions { color :: (Float, Float, Float) }
sprOptions = SpriteOptions { color = (1, 1, 1)}

data SpriteInstance = SpriteInstance { name :: String, x :: Float, y :: Float, options :: SpriteOptions }

spr name x y = SpriteInstance name (fromIntegral x) (fromIntegral y) sprOptions

sprEx name x y options = SpriteInstance name (fromIntegral x) (fromIntegral y) options


--run :: [String] -> IO state -> (state -> state) -> (state -> [SpriteInstance]) -> IO()
run game = do
    (progname, _) <- GL.getArgsAndInitialize
    window <- initGL
    spriteImages <- getAndCreateTextures "data"
    sprites <- fromList hashString []
    mapM_ (\(name, tex) -> insert sprites name tex) spriteImages
 
    startState <- load game
    gameEnv <- newIORef $ GameEnv 0 $ startState
    GL.perWindowKeyRepeat $= GL.PerWindowKeyRepeatOff
    GL.displayCallback $= (display gameEnv sprites (render game))
    GL.idleCallback $= Just (idle gameEnv (move game))
    GL.keyboardMouseCallback $= Just (keyboardMouse window gameEnv (handleInput game))
    GL.mainLoop
  
quit window = do 
    GL.destroyWindow window
    exitWith ExitSuccess 
   
display gameEnv sprites draw = do 
    GameEnv time state  <- GL.get gameEnv
 
    GL.blendEquation $= GL.FuncAdd 
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.blend $= GL.Enabled
 
    GL.textureFunction $= GL.Modulate
    GL.texture GL.Texture2D $= GL.Enabled
    GL.depthFunc $= Nothing
    GL.cullFace $= Nothing

    GL.clearColor $= GL.Color4 0.0 0.0 0.0 (0.0 :: Float)
    GL.clear [GL.ColorBuffer]

    GL.loadIdentity
    GL.ortho 0.0 800 600 0.0 (-1.0) (1.0)
 

    mapM_ (renderSprite sprites) (draw state)

    GL.renderPrimitive GL.Points $ do
            GL.vertex   (GL.Vertex2 0 (0 :: Float))
    GL.swapBuffers
    GL.flush

 
renderSprite :: HashTable String Tex -> SpriteInstance -> IO()
renderSprite textures (SpriteInstance name x y options) = do
        texture <- Data.HashTable.lookup textures name
        let (r,g,b) = color options
        GL.color $ GL.Color4 r g b 1
        case texture of
            Just (Tex textureID w h) -> do
                GL.textureBinding GL.Texture2D $= Just textureID
                GL.renderPrimitive GL.Quads $ do
                    GL.texCoord (GL.TexCoord2 0 (1 :: Float))
                    GL.vertex   (GL.Vertex2 x y)
                    GL.texCoord (GL.TexCoord2 0 (0 :: Float))
                    GL.vertex   (GL.Vertex2 x (y + h))
                    GL.texCoord (GL.TexCoord2 1 (0 :: Float))
                    GL.vertex   (GL.Vertex2 (x + w) (y + h))
                    GL.texCoord (GL.TexCoord2 1 (1 :: Float))
                    GL.vertex   (GL.Vertex2 (x + w) y)
            Nothing -> error $ "Can't find texture " ++ name

idle gameEnv move = do
    env <- GL.get gameEnv
    time <- GL.get GL.elapsedTime
    gameEnv $= tick time env move
    GL.postRedisplay Nothing

keyboardMouse window _ controlsHandler (GL.Char '\ESC') GL.Down _ _ = do
    GL.destroyWindow window
    exitWith ExitSuccess

keyboardMouse _ gameEnv controlsHandler key state modifiers position = do
    GameEnv t gameState <- GL.get gameEnv
    gameEnv $= GameEnv t (controlsHandler key state gameState)

data GameEnv gameState = GameEnv Int gameState
               deriving Show
 
stepPeriod = 6

tick :: Int -> GameEnv gameState -> (gameState -> gameState) -> GameEnv gameState
tick tnew (GameEnv 0 state) _ = GameEnv tnew state
tick tnew (GameEnv told state) move =
    let times = (tnew - told) `div` stepPeriod 
        newState = iterate move state !! times
    in GameEnv (told + stepPeriod * times) newState

initGL = do
    GL.initialDisplayMode $= [GL.DoubleBuffered, GL.RGBAMode]
    GL.initialWindowSize $= GL.Size 800 600
    window <- GL.createWindow "Hello World"
    return window

