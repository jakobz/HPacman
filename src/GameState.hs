{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Data.Array
import Data.Maybe
import Data.List
import Data.Function
import qualified Data.Set as Set
import Data.Lens.Lazy
import Data.Lens.Template 
import Engine.Engine
import Data.Fixed
import Vectors

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
    _tiles :: RenderItem,
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
creatureCenterShift = cellSize * 3 / 2
creatureCenterShiftVec = (creatureCenterShift, creatureCenterShift)
deathAnimationLength = 50 :: Int

-- coordinate helpers
wrapCoords (x, y) = (x `mod'` (levelW * cellSize), y `mod'` (levelH * cellSize))

toPixelCoords :: (Int, Int) -> Coord
toPixelCoords c =  scaleVec cellSize $ toFloatVec c

toLevelCoords :: (Float, Float) -> (Int, Int)
toLevelCoords v = truncVec $ scaleVec (1/cellSize) v

-- common helpers 
firstJust = head . catMaybes 

-- Passes a segment thru portals. Returns a list of segments
-- Segment can pass from several portals.
-- Original segment will be returned if there are no portals on the way.
passVecThruPortalImpl state startPoint dir prevExit =
    let getDist (_,dist,_,_,_) = dist
        intersectPortal (Portal enter@(enterV1, enterV2) exit) =
            case segVecIntersectsDetail (startPoint, dir) (enterV1, enterV2 .-. enterV1) of
                Just (v,a,b) -> Just (v,a,b,enter,exit)
                Nothing -> Nothing

        intersections = 
            sortBy (compare `on` getDist) 
            $ filter (\(_,_,_,enter,_) -> enter /= prevExit)
            $ catMaybes
            $ map intersectPortal (portals ^$ state)

        result = case intersections of
                    ((enterVec,a,b,(enterV1,enterV2),exit@(exitV1, exitV2)):_) -> 
                        let enterPortalDir = enterV2 .-. enterV1
                            exitPortalDir = exitV1 .-. exitV2
                            exitPoint = exitV2 .+. scaleVec b exitPortalDir
                            angleChange = vecAngle exitPortalDir - vecAngle enterPortalDir 
                            rotatedDir = rotateVec angleChange dir
                            outDir = normalizeVec $ rotatedDir
                            exitVec = (scaleVec (1-a) rotatedDir) 
                        in if vecLength exitVec == 0 then (startPoint, enterVec, dir) : [(exitPoint, (0,0), outDir)]
                            else (startPoint, enterVec, normalizeVec dir) : passVecThruPortalImpl state exitPoint exitVec exit
                    otherwise -> [(startPoint, dir, normalizeVec dir)]
    in result

passVecThruPortal state start dir = take 10 $ passVecThruPortalImpl state start dir ((0,0),(0,0))

