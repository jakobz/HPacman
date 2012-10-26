module Rendering (renderGame) where

import Engine
import GameState
import Data.Lens.Lazy
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

renderGhost ghost@(Creature{_coords = coords, _direction = dir}) ghostN =
  renderCreature ("ghost_" ++ [getDirName dir]) coords (ghostsColors !! ghostN)

playerColor Dead = (1,0,0)
playerColor (DeathAnimation n) = 
	let phase = (fromIntegral n) / (fromIntegral deathAnimationLength)
	in (1, phase, phase)
playerColor Alive = (1,1,1)

renderGameSnapshot state =	
	let ps = playerState ^$ state
    in [spr backSpr 0 0]
    	++ [spr "point" (x*cellSize) (y*cellSize) | (x, y) <- Set.elems $ food ^$ state]
    	++ renderPlayer (player ^$ state) (playerColor ps)
    	++ (concat $ zipWith renderGhost (ghosts ^$ state) [0..])

renderGame state = renderGameSnapshot $ head $ states ^$ state