module Rendering (renderGame) where

import Engine
import GameState
import Data.Lens.Lazy
import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Set as Set

backSpr = "back"
point = "point"

getDirName (-1, 0) = 'l'
getDirName (1, 0) = 'r'
getDirName (0, -1) = 'u'
getDirName (0, 1) = 'd'
getDirName (0, 0) = 'r'

whiteColor = (1,1,1)
ghostsColors = [(1,0,0), (1,0,1), (0,1,1), (1,0.5,0)]

itemSpr name (x, y) col = let opts = sprOptions { color = col }
                          in  sprEx name x y opts

getMovePhase (x, y) = "23344311" !! (((x + y) `div` 4) `mod` 8)

renderCreature name c col = [itemSpr name c col, itemSpr name (fstLens ^!-= levelW * cellSize $ c) col]

renderPlayer player@(Creature{_coords = coords, _direction = dir}) =
  renderCreature ("pacman_" ++ [getDirName dir, getMovePhase coords]) coords

renderGhost ghost ghostN =
    let dirName = getDirName (direction ^$ ghost)
        ghostCoords = (coords ^$ ghost) .+. (5,2)
        eye1shift 'l' = (-2, 5)
        eye1shift 'r' = (24, 5)
        eye1shift 'u' = (4, 5)
        eye1shift 'd' = (4, 5) 
        eye1c = (eye1shift dirName) .+. ghostCoords
        eye2shift 'l' = (8, 5)
        eye2shift 'r' = (14, 5) 
        eye2shift 'u' = (17, 5)
        eye2shift 'd' = (17, 5)
        eye2c = (eye2shift dirName) .+. ghostCoords
        (dx, dy) = (target ^$ ghost) .-. ghostCoords .+. (13, 13)
        limit n = max (-2) $ min 2 (n `div` 50)
        eyeVector = (limit dx, limit dy)
        pupil1c = eye1c .+. (5,5) .+. eyeVector
        pupil2c = eye2c .+. (5,5) .+. eyeVector
    in [itemSpr ("ghost" ++ [dirName]) ghostCoords (ghostsColors !! ghostN)
       , itemSpr "eyeballl" eye1c whiteColor
       , itemSpr "pupill" pupil1c whiteColor
       , itemSpr "eyeballr" eye2c whiteColor
       , itemSpr "pupilr" pupil2c whiteColor]
       

playerColor Dead = (1,0,0)
playerColor (DeathAnimation n) = 
	let phase = (fromIntegral n) / (fromIntegral deathAnimationLength)
	in (1, phase, phase)
playerColor Alive = (1,1,1)

renderWorld :: World -> [SpriteInstance]
renderWorld state =	
    let ps = playerState ^$ state
        levelTiles = (tiles.level) ^$ state
        foodSprs = [sprEx "level" (x*cellSize) (y*cellSize) sprOptions{tile = Just 0}
                   | (x, y) <- Set.elems $ food ^$ state]
    in 
      levelTiles
      ++ foodSprs
    	++ renderPlayer (player ^$ state) (playerColor ps)
    	++ (concat $ zipWith renderGhost (ghosts ^$ state) [0..])

renderGame state = renderWorld $ head $ worldStates ^$ state

testTiles = map (\n -> sprEx "level" (n * 20) 10 sprOptions{tile = Just n}) [0..15]