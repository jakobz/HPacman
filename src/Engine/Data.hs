module Engine.Data where

import qualified Graphics.UI.GLUT as GL
import Data.HashTable
import Engine.Vbo

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

data Tex = Tex { textureObject :: GL.TextureObject, width, height :: Float }
            deriving Show

data RenderItem = 
    SpriteInstance { name :: String, x :: Float, y :: Float, options :: SpriteOptions }
    | LineInstance { points :: [(Float, Float)], lineColor :: (Float, Float, Float) }
    | Batch { 
        vbo :: Vbo,
        textureName :: String 
      }

data SpriteOptions = SpriteOptions {
        sprColor :: (Float, Float, Float),
        tile :: Maybe Int,
        rot :: Int
    }