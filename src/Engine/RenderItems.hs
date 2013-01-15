module Engine.RenderItems where 

import Graphics.Rendering.OpenGL
import Engine.Vbo
import Engine.Data
import qualified Data.Map as Map
import Control.Monad
import Engine.Shaders

-- Render items construction

spr name (x, y) = SpriteInstance name x y sprOptions

sprOptions = SpriteOptions { sprColor = (1, 1, 1), tile = Nothing, rot = 0 }

sprEx name (x, y) options = SpriteInstance name x y options

line color points = LineInstance points color    

tileSize = 16

prepareBatch :: [RenderItem] -> IO RenderItem
prepareBatch items = do 
    let toCoords (Vertex2 x y,TexCoord2 tx ty) = [[x,y],[tx,ty]]
    let sprCoords = map toCoords $ concat $ map (fst . spriteToCoords (128, 48)) items
    vbo <- createStaticVbo [2,2] Quads sprCoords

    let textureName = name $ head items
   
    putStrLn $ "Batch prepared on texture '" ++ textureName ++ "', " ++ show (length sprCoords) ++ " vertexes."

    return $ Batch { textureName, vbo, shaderName = "default" }

-- Rendering

toGLColor :: (Float, Float, Float) -> Color4 GLfloat
toGLColor (r,g,b) = Color4 (realToFrac r) (realToFrac g) (realToFrac b) 1

toGLVertex :: (Float, Float) -> Vertex2 GLfloat
toGLVertex (x,y) = Vertex2 (realToFrac x) (realToFrac y) 

renderItem :: Resources -> RenderItem -> IO()

renderItem Resources{textures, shaders} Batch{textureName, vbo, shaderName} = do
    let Just textureResource@(Tex textureID _ _) = Map.lookup textureName textures  
    let Just shader = Map.lookup shaderName shaders 
    textureBinding Texture2D $= Just textureID
    texture Texture2D $= Enabled           
    renderVbo shader vbo

renderItem Resources{textures} sprite@(SpriteInstance name _ _ _) = do
        let maybeTexture = Map.lookup name textures
        case maybeTexture of
            Just textureResource@(Tex textureID texW texH) -> do
                let (coords, sprColor) =
                        spriteToCoords (texW, texH) sprite
                textureBinding Texture2D $= Just textureID
                texture Texture2D $= Enabled
                color $ toGLColor sprColor
                renderPrimitive Quads $ do
                    forM_ coords $ \(c,t) -> do
                        texCoord t
                        vertex   c
            Nothing -> error $ "Can't find texture " ++ name

renderItem textures line@(LineInstance points lineColor) =
    do color $ toGLColor lineColor
       texture Texture2D $= Disabled
       renderPrimitive LineStrip $ do
            forM_ points (\v -> vertex $ toGLVertex v)

spriteToCoords (texWF, texHF) (SpriteInstance name sx sy options) =
    let [x,y,texW,texH] = map realToFrac [sx,sy,texWF,texHF] :: [GLfloat]
        (tx, ty, w, h) = 
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
                            TexCoord2 tleft tbot,
                            TexCoord2 tleft tup,
                            TexCoord2 tright tup,
                            TexCoord2 tright tbot
                        )
        (t1,t2,t3,t4) = case rot options of
                                0 -> (p1,p2,p3,p4)
                                1 -> (p2,p3,p4,p1)
                                2 -> (p3,p4,p1,p2)
                                3 -> (p4,p1,p2,p3) 
        (c1,c2,c3,c4) = (
                (Vertex2 x y),
                (Vertex2 x (y + h)),
                (Vertex2 (x + w) (y + h)),
                (Vertex2 (x + w) y)
            )
    in ([(c1,t1),(c2,t2),(c3,t3),(c4,t4)], sprColor options)