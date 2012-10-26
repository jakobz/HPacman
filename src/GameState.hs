{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Data.Array
import qualified Data.Set as Set
import Data.Lens.Lazy
import Data.Lens.Template 

type Coord = (Int, Int) 

data BlockType = Space | Wall | Food | Drug deriving (Eq)

data Game = Game {
                walls :: Array (Int, Int) Bool,
                _food :: Set.Set (Int, Int),
                _player :: Creature,
                _ghosts :: [Creature]
            }

data Creature = Creature {
                _coords :: Coord,
                _direction :: Coord,
                _intention :: Coord
            }

$( makeLenses [''Game, ''Creature] )    

cellSize, levelW, levelH :: Int
cellSize = 16
levelW = 41
levelH = 37
levelWC = [0 .. levelW-1] 
levelHC = [0 .. levelH-1]

creatureSize = cellSize * 3

readBlock '#' = Wall
readBlock 'p' = Food
readBlock '+' = Drug
readBlock  _  = Space
 
makeCreature x y = Creature (x * cellSize, y * cellSize) (0,0) (0,0)

loadGame = do
  levelData <- readFile "data\\level.txt"
  let level = [ ((x,y), readBlock v)
                      | (y, line) <- zip levelHC $ lines levelData
                      , (x, v) <- zip levelWC line]

  return Game { 
    _player = makeCreature 1 1,
    _ghosts = [makeCreature 17 1, makeCreature 1 33, makeCreature 17 25], --makeCreature 21 19],
    walls = array ((0,0),(levelW-1, levelH-1)) $ map (\(c, b) -> (c, b == Wall)) level,
    _food = Set.fromList $ map fst $ filter (\(_, b) -> b == Food) level
  }
