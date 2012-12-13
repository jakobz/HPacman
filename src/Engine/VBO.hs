module Engine.VBO where

import Data.Array.Storable
import Graphics.Rendering.OpenGL 

data VBO = VBO {
        buffer :: BufferObject,
        bufferSize :: GLuint, 
        primitiveMode :: PrimitiveMode
    }

createVBO = do
    let size = 10000
    [buffer] <- genObjectNames 1
    arr <- newArray (0, size - 1) (0 :: GLfloat)

    bindBuffer ArrayBuffer $= Just buffer                     
    withStorableArray arr (\ptr -> bufferData ArrayBuffer $= (toEnum size, ptr, StaticDraw))
    bindBuffer ArrayBuffer $= Nothing   

    return VBO {
            buffer,
            bufferSize = fromIntegral size,
            primitiveMode = Quads
        }

--drawVBO = do
--    let stride = toEnum sizeOfVertexInfo
--    let vxDesc = VertexArrayDescriptor 2 Float stride $ offset 0
--    let colors = VertexArrayDescriptor 4 Float stride $ offset 8
--    let texCoo = VertexArrayDescriptor 2 Float stride $ offset ( + 16)
--    let filt   = VertexArrayDescriptor 4 Float stride $ offset (12 + 16 + 8)


