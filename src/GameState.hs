{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Data.Array
import qualified Data.Set as Set
import Data.Lens.Lazy
import Data.Lens.Template 
import Engine

type Coord = (Int, Int) 

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
cellSize, levelW, levelH :: Int
cellSize = 16
levelW = 45
levelH = 37
levelWC = [0 .. levelW - 1] 
levelHC = [0 .. levelH - 1]

creatureSize = cellSize * 3

deathAnimationLength = 50 :: Int

-- coordinate helpers
wrapCoords (x, y) = (x `mod` (levelW * cellSize), y `mod` (levelH * cellSize))
toLevelCoords (x, y) = ((x `div` cellSize) `mod` levelW, (y `div` cellSize) `mod` levelH)
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)
vecLength (x, y) = sqrt $ fromIntegral $ x * x + y * y
scaleVec (x, y) scale = (x * scale, y * scale)

