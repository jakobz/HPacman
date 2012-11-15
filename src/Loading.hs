{-# LANGUAGE TemplateHaskell #-}

module Loading(loadGame) where

import Data.Array
import Data.List
import Data.Function
import Text.XML.Light
import Text.XML.Light.Proc
import Data.Maybe
import Engine
import Data.Lens.Lazy
import Data.Lens.Template 
import qualified Data.Set as Set
import qualified Data.Map as Map

import GameState

testLoad = do
            file <- readFile "C:\\Coding\\HPacman\\data\\level.xml"
            return $ parseLevel file

xName name = QName name Nothing Nothing

readIntAttr tag name = read $ fromJust $ findAttr (xName name) tag :: Int

parseLevel txt = 
    let root = head $ onlyElems $ parseXML txt
        
        [w,h,tileSize] = map (readIntAttr root) ["tileswide", "tileshigh", "tilewidth"]

        readTile node = 
            let [x, y, index, rot] = map (readIntAttr node) ["x", "y", "index", "rot"]
            in (x,y,index,rot)

        readLayer node = map readTile $ elChildren node

        layers = map readLayer $ elChildren root

        getTileNum (_,_,tile,_) = tile

        removeEmptyTiles = filter (\t -> getTileNum t >= 0)

        rawTileToWall (x,y,i,_) = ((x,y),i >= 0)
        walls = array ((0,0),(w-1,h-1)) $ map rawTileToWall (layers !! 1)

        rawTileToSpr (x,y,i,r) = 
                sprEx "level" (x * tileSize) (y * tileSize)
                $ sprOptions {tile = Just i, rot = r}

        tilesToRender = map rawTileToSpr 
                        $ removeEmptyTiles 
                        $ concat $ (take 2 $ layers) ++ [layers !! 3]

        layerToSet layer = Set.fromList $ map (\(x,y,_,_) -> (x,y)) layer

        food = layerToSet $ removeEmptyTiles $ layers !! 2

        -- Parse portals. Portals are represented as tiles at layer #3
        -- Each portal has 2 enters which are formed with distinct tiles 

        portalGroups = 
                    map layerToRotMap
                    $ groupBy ((==) `on` getTileNum)
                    $ sortBy (compare `on` getTileNum) 
                    $ removeEmptyTiles $ layers !! 3

        layerToRotMap layer = Map.fromList $ map (\(x,y,_,rot) -> ((x,y),rot)) layer

        extractPortal enterTiles = 
            let 
                -- Each enter is formed with 3 tiles for editing conviniencs, 
                -- We considering only topmost leftmost of each 3 during parsing
                isVisualExt ((x,y), _) =
                    not (Map.member (x-1, y) enterTiles || Map.member (x, y-1) enterTiles)
                distinctEnters = filter isVisualExt $ 
                        Map.toList enterTiles
                enters = map (\(c, rot) -> PortalEnter c rot) distinctEnters
            in Portal enters

        portals = map extractPortal portalGroups

    in Level tilesToRender walls food portals

loadGame = do
  levelXml <- readFile "data\\level.xml"
  let level = parseLevel levelXml
      makeCreature x y = Creature (x * cellSize, y * cellSize) (0,0) (0,0) (0,0)
      initGame = World { 
        _level = level,
        _player = makeCreature 1 1,
        _ghosts = [makeCreature 41 1, makeCreature 1 33, makeCreature 19 17, makeCreature 41 33],
        _food = initialFood ^$ level,
        _playerState = Alive
      }
  return Game {
        _worldStates = [initGame],
        _timeDirection = Normal,
        _playerIntention = (0,0)
      }