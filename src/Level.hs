{-# LANGUAGE TemplateHaskell #-}

module Level(Level, parseLevel, tiles, walls, initialFood) where

import Data.Array
import Data.List
import Text.XML.Light
import Text.XML.Light.Proc
import Data.Maybe
import Engine
import Data.Lens.Lazy
import Data.Lens.Template 
import qualified Data.Set as Set

data Level = Level {
    _tiles :: [SpriteInstance],
    _walls :: Array (Int, Int) Bool,
    _initialFood :: Set.Set (Int, Int)
}

$( makeLenses [''Level] )    

testLoad = do
            file <- readFile "C:\\Coding\\HPacman\\data\\level.xml"
            return $ parseLevel file

xName name = QName name Nothing Nothing

readIntAttr tag name = read $ fromJust $ findAttr (xName name) tag :: Int

parseLevel txt = 
    let root = head $ onlyElems $ parseXML txt
        
        [w,h,tileSize] = map (readIntAttr root) ["tileswide", "tileshigh", "tilewidth"]
        layers = map readLayer $ elChildren root

        readTile node = 
            let [x, y, index, rot] = map (readIntAttr node) ["x", "y", "index", "rot"]
            in (x,y,index,rot)

        readLayer node = map readTile $ elChildren node

        rawTileToWall (x,y,i,_) = ((x,y),i >= 0)
        walls = array ((0,0),(w-1,h-1)) $ map rawTileToWall (layers !! 1)

        rawTileToSpr (x,y,i,r) = 
                sprEx "level" (x * tileSize) (y * tileSize)
                $ sprOptions {tile = Just i, rot = r}

        tilesToRender = concatMap (map rawTileToSpr) $ take 2 $ layers

        food = Set.fromList
                    $ map (\(x,y,_,_) -> (x,y))
                    $ filter (\(_,_,i,_) -> i >= 0) $ layers !! 2

    in Level tilesToRender walls food
