module Rendering (renderGame) where

import Engine.Engine
import GameState
import Vectors
import Data.Lens.Lazy
import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Set as Set

backSpr = "back"
point = "point"

getDirName (-1, _) = 'l'
getDirName (1, _) = 'r'
getDirName (_, -1) = 'u'
getDirName (_, 1) = 'd'
getDirName _ = 'r'

whiteColor = (1,1,1)
ghostsColors = [(1,0,0), (1,0,1), (0,1,1), (1,0.5,0)]

itemSpr name c col = let opts = sprOptions { sprColor = col }
                          in  sprEx name c opts

getMovePhase (x, y) = "23344311" !! (((truncate $ x + y) `div` 4) `mod` 8)

renderCreature name c col = [itemSpr name c col]

renderPlayer player@(Creature{_direction = dir}) =
  let c = (coords ^$ player) .-. creatureCenterShiftVec
  in renderCreature ("pacman_" ++ [getDirName dir, getMovePhase c]) c

renderGhost ghost ghostN =
    let dirName = getDirName (direction ^$ ghost)
        ghostCoords = (coords ^$ ghost) .-. creatureCenterShiftVec .+. (5,2) 
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
        limit n = max (-2) $ min 2 (n / 50)
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

renderWorld :: World -> [RenderItem]
renderWorld state =	
    let ps = playerState ^$ state
        levelTiles = (tiles.level) ^$ state
        foodSprs = [sprEx "level" (scaleVec cellSize $ toFloatVec v) sprOptions{tile = Just 0}
                   | v <- Set.elems $ food ^$ state]
    in 
      levelTiles
      ++ foodSprs
    	++ renderPlayer (player ^$ state) (playerColor ps)
    	++ (concat $ zipWith renderGhost (ghosts ^$ state) [0..])
      -- ++ renderPortals (level ^$ state)
      -- ++ testPortals state
      -- ++ testPortals2 state


renderGame state = renderWorld $ head $ worldStates ^$ state


-- debug rendering
renderPortals level = 
  let allPortals = portals ^$ level
      portalToLine (Portal (p1,p2) _) = line (1,1,1) [p1, p2]
  in map portalToLine allPortals


testPortals state =
  let lvl = (level ^$ state)
      playerC = ((coords.player) ^$ state)
      playerD = scaleVec 1000 ((direction.player) ^$ state)
      lines = passVecThruPortal lvl playerC (rotateVec (-0.1) playerD)
              ++ passVecThruPortal lvl playerC (rotateVec (0) playerD)
              ++ passVecThruPortal lvl playerC (rotateVec (0.1) playerD)
  in map (\(s,e,_) -> line (1,1,1) [s,s .+. e]) lines

cp = [-creatureCenterShift + 0.5, -(cellSize/2), cellSize/2, creatureCenterShift - 0.5]

pts = [(dx, dy) | dx <- cp, dy <- cp] 

testPortals2 state =
  let lvl = (level ^$ state)
      playerC = ((coords.player) ^$ state)
      lines = concatMap (passVecThruPortal lvl playerC) pts
  in map (\(s,e,_) -> line (1,1,1) [s,s .+. e]) lines

