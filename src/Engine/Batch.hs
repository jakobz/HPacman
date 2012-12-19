module Engine.Batch where

import Data.Array.Storable
import Graphics.Rendering.OpenGL 
import Foreign.Ptr
import Debug.Trace
import Graphics.UI.GLUT (reportErrors)
import Control.Monad
import System.Random

data Batch = Batch {
        buffer :: BufferObject,
        bufferSize :: GLsizei, 
        bufferArray :: StorableArray Int GLfloat,
        primitiveMode :: PrimitiveMode
    }

createBatch = do
    let size = 200000
    [buffer] <- genObjectNames 1
    points <- replicateM size (randomRIO (0,1000) :: IO GLfloat)
    bufferArray <- newListArray (0, size - 1) points

    bindBuffer ArrayBuffer $= Just buffer                     
    withStorableArray bufferArray (\ptr -> bufferData ArrayBuffer $= (toEnum size, ptr, StaticDraw))
    bindBuffer ArrayBuffer $= Nothing   

    reportErrors

    return Batch {
            buffer,
            bufferSize = fromIntegral size,
            primitiveMode = Quads,
            bufferArray
        }

renderBatch Batch{buffer, bufferSize, bufferArray, primitiveMode} = 
    withStorableArray bufferArray $ \arrayPtr -> do    
        let elemSize = 4
        let buffOffset n = plusPtr arrayPtr (n * elemSize)
        let stride = 0
        let vxDesc = VertexArrayDescriptor 2 Float stride $ buffOffset 0
        
        -- test
        blend $= Disabled
        color $ Color4 1 0 0 (1 :: GLfloat)
        textureBinding Texture2D $= Nothing
        texture Texture2D $= Disabled

        bindBuffer ArrayBuffer $= Just buffer
        clientState VertexArray $= Enabled
        arrayPointer VertexArray $= vxDesc

        drawArrays primitiveMode 0 bufferSize
        clientState VertexArray $= Disabled
        bindBuffer ArrayBuffer $= Nothing

        reportErrors

