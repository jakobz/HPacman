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
    _tiles :: [RenderItem],
    _walls :: Array (Int, Int) Bool,
    _initialFood :: Set.Set (Int, Int),
    _portals :: [Portal]
} 

data Portal = Portal {
    _enter :: (Coord, Coord),
    _exit :: (Coord, Coord)
} deriving (Show)

$( makeLenses [''Game, ''World, ''Creature, ''Level, ''Portal] )    

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

toPixelCoords :: (Int, Int) -> Coord
toPixelCoords c =  scaleVec cellSize $ toFloatVec c

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

(x1,y1) .*. (x2,y2) = x1 * y2 - y1 * x2

-- algorythm described here: http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
segmentsIntersects :: (Coord, Coord) -> (Coord, Coord) -> Maybe Coord
segmentsIntersects (s1,e1) (s2,e2) = 
    let p = s1
        r = e1 .-. s1
        q = s2
        s = e2 .-. s2
    in segVecIntersects (p,r) (q,s)

segVecIntersects (p,r) (q,s) = 
    let rs = r .*. s
    in if (rs == 0) 
        then Nothing 
        else let 
                qp = (q .-. p) 
                t = (qp .*. s) / rs
                u = (qp .*. r) / rs
            in if (0 <= t && t <= 1 && 0 <= u && u <= 1) 
                then Just $ p .+. scaleVec t r
                else Nothing


directionToVec 0 = (-1, 0)
directionToVec 1 = ( 0, 1)
directionToVec 2 = ( 1, 0)
directionToVec 3 = ( 0,-1)