module Engine.Sprites where 

import qualified Graphics.UI.GLUT as GL

data RenderItem = 
    SpriteInstance { name :: String, x :: Float, y :: Float, options :: SpriteOptions }
    | LineInstance { points :: [(Float, Float)], lineColor :: (Float, Float, Float) }

data SpriteOptions = SpriteOptions {
        sprColor :: (Float, Float, Float),
        tile :: Maybe Int,
        rot :: Int
    }

sprOptions = SpriteOptions { sprColor = (1, 1, 1), tile = Nothing, rot = 0 }

spr name (x, y) = SpriteInstance name x y sprOptions

sprEx name (x, y) options = SpriteInstance name x y options

line color points = LineInstance points color    

tileSize = 16

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
                            GL.TexCoord2 tleft tbot,
                            GL.TexCoord2 tleft tup,
                            GL.TexCoord2 tright tup,
                            GL.TexCoord2 tright tbot
                        )
        (t1,t2,t3,t4) = case rot options of
                                0 -> (p1,p2,p3,p4)
                                1 -> (p2,p3,p4,p1)
                                2 -> (p3,p4,p1,p2)
                                3 -> (p4,p1,p2,p3) 
        (c1,c2,c3,c4) = (
                (GL.Vertex2 x y),
                (GL.Vertex2 x (y + h)),
                (GL.Vertex2 (x + w) (y + h)),
                (GL.Vertex2 (x + w) y)
            )
    in ((c1,c2,c3,c4), (t1,t2,t3,t4), sprColor options)