{-# LANGUAGE TemplateHaskell #-}

module Engine (newGame, run, spr, sprEx, load, move, render, handleInput, sprOptions, color, tile) where

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

data SpriteOptions = SpriteOptions {
        color :: (Float, Float, Float),
        tile :: Maybe Int
    }

sprOptions = SpriteOptions { color = (1, 1, 1), tile = Nothing }

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

tileSize = 16
 
renderSprite :: HashTable String Tex -> SpriteInstance -> IO()
renderSprite textures (SpriteInstance name x y options) = do
        texture <- Data.HashTable.lookup textures name       
        let (r,g,b) = color options
        GL.color $ GL.Color4 r g b 1
        case texture of
            Just (Tex textureID texW texH) -> do
                let (tx, ty, w, h) = case tile options of 
                                        Just tileN -> let tilesPerLine = (truncate texW) `div` tileSize
                                                          tilesLines = (truncate texH) `div` tileSize
                                                          (tileY, tileX) = (toInteger tileN) `divMod` tilesPerLine
                                                      in (fromIntegral $ tileX * tileSize, 
                                                          fromIntegral $ (tilesLines - tileY - 1) * tileSize, 
                                                          fromIntegral tileSize,
                                                          fromIntegral tileSize)
                                        Nothing -> (0, 0, texW, texH)
                GL.textureBinding GL.Texture2D $= Just textureID
                let scaleTX c = c / texW
                let scaleTY c = c / texH
                let tleft = scaleTX tx
                let tright = scaleTX (tx + w)
                let tup = scaleTY ty
                let tbot = scaleTY (ty + h)
                GL.renderPrimitive GL.Quads $ do
                    GL.texCoord (GL.TexCoord2 tleft tbot)
                    GL.vertex   (GL.Vertex2 x y)
                    GL.texCoord (GL.TexCoord2 tleft tup)
                    GL.vertex   (GL.Vertex2 x (y + h))
                    GL.texCoord (GL.TexCoord2 tright tup)
                    GL.vertex   (GL.Vertex2 (x + w) (y + h))
                    GL.texCoord (GL.TexCoord2 tright tbot)
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
    window <- GL.createWindow "HPacman"
    return window

