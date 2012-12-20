module Engine.Vbo where

import Data.Array.Storable
import Graphics.Rendering.OpenGL 
import Foreign.Ptr
import Debug.Trace
import Graphics.UI.GLUT (reportErrors)
import Control.Monad
import System.Random

data Vbo = Vbo {
        buffer :: BufferObject,
        bufferSize :: GLsizei, 
        bufferArray :: StorableArray Int GLfloat,
        primitiveMode :: PrimitiveMode
    }

createVbo = do
    let size = 200000
    points <- replicateM size (randomRIO (0, 1000) :: IO GLfloat)
    bufferArray <- newListArray (0, size - 1) points

    -- create buffer object 
    [buffer] <- genObjectNames 1 
    -- initialize the buffer
    bindBuffer ArrayBuffer $= Just buffer 
    -- copy data to the buffer from the bufferArray
    withStorableArray bufferArray (\ptr -> bufferData ArrayBuffer $= (toEnum size, ptr, StreamRead))

    -- unbind the buffer
    bindBuffer ArrayBuffer $= Nothing

    reportErrors

    return Vbo {
            buffer,
            bufferSize = fromIntegral size,
            primitiveMode = Quads,
            bufferArray
        }

renderVbo Vbo{buffer, bufferSize, bufferArray, primitiveMode} = 
    withStorableArray bufferArray $ \arrayPtr -> do    
        let elemSize = 4
        let buffOffset n = plusPtr nullPtr (n * elemSize)
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

