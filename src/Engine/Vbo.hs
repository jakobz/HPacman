module Engine.Vbo where

import Data.Array.Storable
import Graphics.Rendering.OpenGL 
import Foreign.Ptr
import Debug.Trace
import Graphics.UI.GLUT (reportErrors)
import Control.Monad
import System.Random
import Engine.Shaders

data Vbo = Vbo {
        buffer :: BufferObject,        
        itemsCount :: GLsizei, 
        bufferArray :: StorableArray Int GLfloat,        
        primitiveMode :: PrimitiveMode,
        descriptors :: [VertexArrayDescriptor GLfloat]
    }

floatSize = 4

createStaticVbo components primitiveMode items = do 
    let itemSize = sum components
    let count = length items
    let points = concat $ concat items
    let buffOffset n = plusPtr nullPtr (n * floatSize)
    let stride = fromIntegral $ itemSize * floatSize
    let size = count * itemSize * floatSize
    let comonentsShifts = scanl (+) 0 [2,2,1]
    let descriptors = zipWith 
            (\shift size -> VertexArrayDescriptor 2 Float stride $ buffOffset shift) 
            comonentsShifts components

    bufferArray <- newListArray (0, (count * itemSize) - 1) points

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
    withStorableArray bufferArray (\ptr -> bufferData ArrayBuffer $= (toEnum size, ptr, StaticDraw))

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

renderVbo shader Vbo{buffer, itemsCount, bufferArray, primitiveMode, descriptors} = do
    let attrNames = ["position", "uv"]

    currentProgram $= Just shader
    bindBuffer ArrayBuffer $= Just buffer

    forM_ (zip3 [1..] descriptors attrNames) $ \(n, descriptor, name) -> do
        attr <- get (attribLocation shader name)
        vertexAttribPointer attr $= (ToFloat, descriptor)
        vertexAttribArray attr $= Enabled

    drawArrays primitiveMode 0 itemsCount

    bindBuffer ArrayBuffer $= Nothing
    currentProgram $= Nothing

    reportErrors

