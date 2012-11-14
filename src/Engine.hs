{-# LANGUAGE TemplateHaskell #-}

module Engine (newGame, run, spr, sprEx, load, move, render, handleInput,
                SpriteInstance(), sprOptions, color, tile, rot) where

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
        tile :: Maybe Int,
        rot :: Int
    }

sprOptions = SpriteOptions { color = (1, 1, 1), tile = Nothing, rot = 0 }

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
    GL.texture GL.Texture2D $= GL.Enabled
    GL.depthFunc $= Nothing
    GL.cullFace $= Nothing


    mapM_ (renderSprite sprites) (draw state)

    GL.renderPrimitive GL.Points $ do
            GL.vertex   (GL.Vertex2 0 (0 :: Float))
    GL.swapBuffers
    GL.flush

tileSize = 16

spriteToCoords (texW, texH) (SpriteInstance name x y options) =
    let (tx, ty, w, h) = 
            case tile options of 
                Just tileN -> let tilesPerLine = (truncate texW) `div` tileSize
                                  tilesLines = (truncate texH) `div` tileSize
                                  (tileY, tileX) = (toInteger tileN) `divMod` tilesPerLine
                              in (fromIntegral $ tileX * tileSize, 
                                  fromIntegral $ (tilesLines - tileY - 1) * tileSize, 
                                  fromIntegral tileSize,
                                  fromIntegral tileSize)
                Nothing -> (0, 0, texW, texH)
        scaleTX c = c / texW 
        scaleTY c = c / texH 
        tleft = scaleTX tx
        tright = scaleTX (tx + w)
        tup = scaleTY ty
        tbot = scaleTY (ty + h)
        (p1,p2,p3,p4) = (
                            GL.TexCoord2 tleft tbot,
                            GL.TexCoord2 tleft tup,
                            GL.TexCoord2 tright tup,
                            GL.TexCoord2 tright tbot
                        )
        (t1,t2,t3,t4) = case rot options of
                                0 -> (p1,p2,p3,p4)
                                1 -> (p2,p3,p4,p1)
                                2 -> (p3,p4,p1,p2)
                                3 -> (p4,p1,p2,p3) 
        (c1,c2,c3,c4) = (
                (GL.Vertex2 x y),
                (GL.Vertex2 x (y + h)),
                (GL.Vertex2 (x + w) (y + h)),
                (GL.Vertex2 (x + w) y)
            )
    in ((c1,c2,c3,c4), (t1,t2,t3,t4), color options)
 
renderSprite :: HashTable String Tex -> SpriteInstance -> IO()
renderSprite textures sprite@(SpriteInstance name _ _ _) = do
        maybeTexture <- Data.HashTable.lookup textures name  
        case maybeTexture of
            Just texture@(Tex textureID texW texH) -> do
                let ((c1,c2,c3,c4), (t1,t2,t3,t4), (r,g,b)) =
                        spriteToCoords (texW, texH) sprite
                GL.textureBinding GL.Texture2D $= Just textureID
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

