module Engine.RenderItems where 

import Graphics.UI.GLUT
import Engine.Vbo
import Engine.Data
import Data.HashTable
import Control.Monad

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
   
    return $ Batch {
        textureName = name $ head items,
        vbo
    }

-- Rendering

renderItem :: Resources -> RenderItem -> IO()

renderItem Resources{textures} Batch{textureName, vbo} = do
    maybeTexture <- Data.HashTable.lookup textures textureName 
    case maybeTexture of 
        Just textureResource@(Tex textureID _ _) -> do
            textureBinding Texture2D $= Just textureID
            texture Texture2D $= Enabled           
            renderVbo vbo

renderItem Resources{textures} sprite@(SpriteInstance name _ _ _) = do
        maybeTexture <- Data.HashTable.lookup textures name  
        case maybeTexture of
            Just textureResource@(Tex textureID texW texH) -> do
                let (coords, (r,g,b)) =
                        spriteToCoords (texW, texH) sprite
                textureBinding Texture2D $= Just textureID
                texture Texture2D $= Enabled
                color $ Color4 r g b 1
                renderPrimitive Quads $ do
                    forM_ coords $ \(c,t) -> do
                        texCoord t
                        vertex   c
            Nothing -> error $ "Can't find texture " ++ name

renderItem textures line@(LineInstance points (r,g,b)) =
    do color $ Color4 r g b 1
       texture Texture2D $= Disabled
       renderPrimitive LineStrip $ do
            forM_ points (\(x,y) -> vertex $ Vertex2 x y)

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