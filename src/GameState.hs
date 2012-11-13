{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Data.Array
import qualified Data.Set as Set
import Data.Lens.Lazy
import Data.Lens.Template 
import Level 

type Coord = (Int, Int) 

data BlockType = Space | Wall | Food | Drug deriving (Eq)

data PlayerState = Alive | DeathAnimation Int | Dead deriving (Eq)

data Game = Game {
                _level :: Level,
                _food :: Set.Set (Int, Int),
                _player :: Creature,
                _ghosts :: [Creature],
                _playerState :: PlayerState
            }

data TimeDirection = Normal | Rewind 

data TimeGame = TimeGame {
				_states :: [Game],
				_timeDirection :: TimeDirection,
				_playerIntention :: Coord
			}

data Creature = Creature {
                _coords :: Coord,
                _direction :: Coord,
                _intention :: Coord,
                _target :: Coord
            }

$( makeLenses [''TimeGame, ''Game, ''Creature] )    

cellSize, levelW, levelH :: Int
cellSize = 16
levelW = 45
levelH = 37
levelWC = [0 .. levelW - 1] 
levelHC = [0 .. levelH - 1]

creatureSize = cellSize * 3

readBlock '#' = Wall
readBlock 'p' = Food
readBlock '+' = Drug
readBlock  _  = Space
 
makeCreature x y = Creature (x * cellSize, y * cellSize) (0,0) (0,0) (0,0)

deathAnimationLength = 50 :: Int

loadGame = do
  levelXml <- readFile "data\\level.xml"
  let level = parseLevel levelXml
  let 
      initGame = Game { 
        _level = level,
        _player = makeCreature 1 1,
        _ghosts = [makeCreature 17 1, makeCreature 1 33, makeCreature 17 25, makeCreature 21 19],
        _food = initialFood ^$ level,
        _playerState = Alive
      }
  return TimeGame {
        _states = [initGame],
        _timeDirection = Normal,
        _playerIntention = (0,0)
      }


-- coordinate helpers
wrapCoords (x, y) = (x `mod` (levelW * cellSize), y `mod` (levelH * cellSize))
toLevelCoords (x, y) = ((x `div` cellSize) `mod` levelW, (y `div` cellSize) `mod` levelH)
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)
vecLength (x, y) = sqrt $ fromIntegral $ x * x + y * y
scaleVec (x, y) scale = (x * scale, y * scale)