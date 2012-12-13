module Engine.Textures (getAndCreateTextures, Tex(Tex)) where

import Graphics.UI.GLUT
import Engine.TGA (readTga)
import Data.Word (Word8)
import Data.Maybe
import Data.List (isSuffixOf)
import System.Directory
import Control.Monad
import Debug.Trace
import System.FilePath.Windows

import Foreign.Marshal.Alloc (free)

data Tex = Tex { texture :: TextureObject, width, height :: Float }
            deriving Show

getImageFiles dir = getCurrentDirectory
              >>= (\d -> return (d ++ "\\" ++ dir))
              >>= getDirectoryContents 
              >>= filterM (return.isSuffixOf ".tga")
              >>= mapM (return.(\d -> dir ++ "\\" ++ d))

-- read a list of images and returns a list of textures
-- all images are assumed to be in the TGA image format
getAndCreateTextures :: String -> IO [(String, Tex)]
getAndCreateTextures subDir = do
   fileNames <- getImageFiles subDir
   texObjs <- mapM getAndCreateTexture fileNames
   return $ zip (map takeBaseName fileNames) texObjs

-- read a single texture
getAndCreateTexture :: String -> IO (Tex)
getAndCreateTexture fileName = do
   putStr $ "Loading " ++ fileName ++ "... "
   texData@((Size w h) , PixelData pixelFormat _ _) <- readImageC fileName
   texObj <- createTexture texData
   putStrLn $ "done " ++ show (w, h, pixelFormat)
   return texObj

-- read the image data
readImageC :: String -> IO (Size, PixelData Word8)
readImageC path = do maybeTga <- readTga path
                     return $ fromJust maybeTga

-- creates the texture
createTexture :: (Size, PixelData a) -> IO (Tex)
createTexture ((Size x y), pixels@(PixelData _ _ ptr)) = do
   [texName] <- genObjectNames 1  -- generate our texture.
   textureBinding Texture2D $= Just texName  -- make our new texture the current texture.

--   build2DMipmaps Texture2D RGBA' (fromIntegral x) (fromIntegral y) pixels
   texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D x y) 0 pixels

   --textureFilter  Texture2D $= ((Nearest, Nothing), Nearest)
   textureFilter  Texture2D $= ((Linear', Nothing), Linear')
   textureWrapMode Texture2D S $= (Repeated, Clamp)
   textureWrapMode Texture2D T $= (Repeated, Clamp)
   free ptr
   return (Tex texName (fromIntegral x) (fromIntegral y))

