{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Data.Array
import Data.Maybe
import qualified Data.Set as Set
import Data.Lens.Lazy
import Data.Lens.Template 
import Engine
import Data.Fixed

type Coord = (Float, Float) 

data BlockType = Space | Wall | Food | Drug deriving (Eq)

data PlayerState = Alive | DeathAnimation Int | Dead deriving (Eq)

data World = World {
                _level :: Level,
                _food :: Set.Set (Int, Int),
                _player :: Creature,
                _ghosts :: [Creature],
                _playerState :: PlayerState
            }

data TimeDirection = Normal | Rewind 

data Game = Game {
				_worldStates :: [World],
				_timeDirection :: TimeDirection,
				_playerIntention :: Coord
			}

data Creature = Creature {
                _coords :: Coord,
                _direction :: Coord,
                _intention :: Coord,
                _target :: Coord
            }

data Level = Level {
    _tiles :: [SpriteInstance],
    _walls :: Array (Int, Int) Bool,
    _initialFood :: Set.Set (Int, Int),
    _portals :: [Portal]
} 

data Portal = Portal {
    _enters :: [PortalEnter]
} deriving (Show)

data PortalEnter = PortalEnter{
    _portalStart :: (Int, Int),
    _portalDirection :: Int
} deriving (Show)            

$( makeLenses [''Game, ''World, ''Creature, ''Level, ''Portal, ''PortalEnter] )    

-- consts
cellSize = 16
levelW = 45
levelH = 37
levelWC = [0 .. levelW - 1] 
levelHC = [0 .. levelH - 1]

creatureSize = cellSize * 3
creatureCenterShift = (cellSize * 3 / 2, cellSize * 3 / 2)
deathAnimationLength = 50 :: Int

-- common helpers 
firstJust = head . catMaybes 

-- coordinate helpers
wrapCoords (x, y) = (x `mod'` (levelW * cellSize), y `mod'` (levelH * cellSize))

toLevelCoords :: (Float, Float) -> (Int, Int)
toLevelCoords v = truncVec $ scaleVec (1/cellSize) $ wrapCoords v

(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)

vecLength :: Coord -> Float
vecLength (x, y) = sqrt $ x * x + y * y
scaleVec scale (x, y) = (x * scale, y * scale)

circlesIntersects :: Float -> Coord -> Coord -> Bool
circlesIntersects size c1 c2 = vecLength (c1 .-. c2) <= size
boxesIntersects ((x1, y1), (sx1, sy1)) ((x2,y2), (sx2, sy2)) = 
    let byX = (abs $ x1 - x2) * 2 <= (sx1 + sx2)
        byY = (abs $ y1 - y2) * 2 <= (sy1 + sy2)
    in byX && byY

truncVec :: Coord -> (Int, Int)
truncVec (x,y) = (truncate x, truncate y)

toFloatVec :: (Int, Int) -> Coord
toFloatVec (x,y) = (fromIntegral x, fromIntegral y)