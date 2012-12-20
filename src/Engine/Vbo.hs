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
        itemsCount :: GLsizei, 
        bufferArray :: StorableArray Int GLfloat,        
        primitiveMode :: PrimitiveMode,
        descriptors :: [VertexArrayDescriptor GLfloat]
    }

floatSize = 4

--createStaticVbo :: [Int] -> PrimitiveMode -> [[[GLfloat]]] -> Vbo
createStaticVbo components primitiveMode items = do 
    let itemSize = sum components
    let count = length items
    let points = concat $ concat items
    let buffOffset n = plusPtr nullPtr (n * floatSize)
    let stride = fromIntegral $ itemSize * floatSize
    let size = count * itemSize * floatSize
    let descriptors = [VertexArrayDescriptor 2 Float stride $ buffOffset 0,
                VertexArrayDescriptor 2 Float stride $ buffOffset 2]

    bufferArray <- newListArray (0, (count * itemSize) - 1) points

    -- TODO

    [buffer] <- genObjectNames 1 
    bindBuffer ArrayBuffer $= Just buffer 
    withStorableArray bufferArray (\ptr -> bufferData ArrayBuffer $= (toEnum size, ptr, StreamRead))
    bindBuffer ArrayBuffer $= Nothing
    
    reportErrors

    return Vbo {
        buffer, itemsCount = fromIntegral count, bufferArray, primitiveMode, descriptors
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
            itemsCount = fromIntegral size,
            primitiveMode = Quads,
            bufferArray,
            descriptors = []
        } 

renderVbo Vbo{buffer, itemsCount, bufferArray, primitiveMode, descriptors} = 
    withStorableArray bufferArray $ \arrayPtr -> do    
        -- test
        --blend $= Disabled
        --color $ Color4 1 0 0 (1 :: GLfloat)
        --textureBinding Texture2D $= Nothing
        --texture Texture2D $= Disabled

        bindBuffer ArrayBuffer $= Just buffer
        clientState VertexArray $= Enabled
        clientState TextureCoordArray $= Enabled
        arrayPointer VertexArray $= descriptors !! 0
        arrayPointer TextureCoordArray $= descriptors !! 1

        drawArrays primitiveMode 0 itemsCount
        clientState VertexArray $= Disabled
        clientState TextureCoordArray $= Disabled
        bindBuffer ArrayBuffer $= Nothing

        reportErrors

