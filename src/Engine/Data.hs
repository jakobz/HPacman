module Engine.Data where

import qualified Graphics.UI.GLUT as GLU
import qualified Graphics.Rendering.OpenGL as GL
import Data.Map
import Engine.Vbo

data App state = App {
        load :: IO state,
        move :: state -> state,
        render :: state -> [RenderItem],
        handleInput ::  GLU.Key -> GLU.KeyState -> state -> state
    }

data EngineState appState = 
    EngineState {
        tick :: Int,
        appState :: appState,
        app :: App appState,
        resources :: Resources
    } 

data Resources = Resources {
    textures :: Map String Tex,
    shaders :: Map String GL.Program
} 

data Tex = Tex { textureObject :: GL.TextureObject, width, height :: Float }
            deriving Show

type Color = (Float, Float, Float)

data RenderItem = 
    SpriteInstance { name :: String, x :: Float, y :: Float, options :: SpriteOptions }
    | LineInstance { points :: [(Float, Float)], lineColor :: Color }
    | Batch { 
        vbo :: Vbo,
        textureName :: String,
        shaderName :: String
      }

data SpriteOptions = SpriteOptions {
        sprColor :: Color,
        tile :: Maybe Int,
        rot :: Int
    }
