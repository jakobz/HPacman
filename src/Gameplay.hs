module Gameplay (moveTimeGame, userAction) where 

import GameState
import Prelude hiding ((.), id)
import Data.Lens.Lazy
import Control.Category
import Data.Array
import qualified Data.Set as Set
import Data.Function (on)
import qualified Graphics.UI.GLUT as GL
import Data.List

-- coordinate helpers
wrapCoords (x, y) = (x `mod` (levelW * cellSize), y `mod` (levelH * cellSize))
toLevelCoords (x, y) = ((x `div` cellSize) `mod` levelW, (y `div` cellSize) `mod` levelH)
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)
vecLength (x, y) = sqrt $ fromIntegral $ x * x + y * y
scaleVec (x, y) scale = (x * scale, y * scale)

-- gameplay
moveGame state =
  let 
    playerCoords = coords.player ^$ state
    playerDir = direction.player ^$ state
    walls = _walls state

    -- collision detection 
    inWall c = walls ! toLevelCoords c
    collisionPoints = [0, cellSize, cellSize * 2, cellSize * 3 - 1]
    boxInWall c = or [inWall $ c .+. (dx, dy) | dx <- collisionPoints, dy <- collisionPoints] 
    canMove c dir = not $ boxInWall (c .+. dir)

    -- physics
    moveCreature creature@(Creature{_coords = coords, _direction = oldDir}) =
        let 
            currentIntention@(keyX, keyY) = intention ^$ creature
            -- try to change direction to one of intended directions
            newDir = case 0 of 
                      _| keyX /= 0 && canMove coords (keyX, 0) -> (keyX, 0)
                      _| keyY /= 0 && canMove coords (0, keyY) -> (0, keyY)
                      _| otherwise -> oldDir
            -- move along the current direction if it is possible
            newCoords = if canMove coords newDir
                          then wrapCoords $ coords .+. newDir
                          else coords
        in Creature newCoords newDir currentIntention

    eat = Set.delete (toLevelCoords $ playerCoords .+. ((cellSize * 3) `div` 2, (cellSize * 3) `div` 2))

    -- enemy AI. Changes intention of the ghost depending on the game state
    ghostAI ghostN ghost = 
      let       
      	-- prepare necessary info
        ghostCoords = coords ^$ ghost
        distToPlayer = vecLength $ playerCoords .-. ghostCoords
        (cdx, cdy) = direction ^$ ghost

        -- detect collision to another ghosts
        otherGhostsCoords = filter (/=ghostCoords) $ map (coords ^$) $ ghosts ^$ state
        creaturesCollide (x1,y1) (x2,y2) = (abs (x1 - x2) <= creatureSize) && (abs (y1 - y2) <= creatureSize)
        ghostCanMove dir = (canMove ghostCoords dir) 
        	&& (not $ any (creaturesCollide $ ghostCoords .+. dir) otherGhostsCoords)

        -- there are two modes: 
        -- - idle - ghosts move to corners 
        -- - attack - ghosts try to catch the player
        (right, bottom) = ((levelW - 1) * cellSize, (levelH - 1) * cellSize)
        idleTargets = [(0, 0), (right, 0), (right, bottom), (0, bottom)] !! ghostN

        -- each ghost have its own attack target
        attackTargets = [
        	-- the red one attacks the player directly
            playerCoords, 
            -- the pink one tries to get in front of the player
            playerCoords .+. scaleVec (direction.player ^$ state) (12 * cellSize),
            -- the blue one tries to get to the back of the player
            playerCoords .-. scaleVec (direction.player ^$ state) (12 * cellSize),
            -- the yellow one tries to move at some distance around the player
            if distToPlayer > 100 then playerCoords else (right `div` 2, bottom `div` 2)
          ]

        target = attackTargets !! ghostN
        (dx, dy) = target .-. ghostCoords
        desiredDirections = 
            sortBy (compare `on` vecLength) 
            $ filter (/= (0, 0)) 
            $ [(0, signum dy), (signum dx, 0)]
        backupDirections = take 4 $ drop ghostN $ cycle [(1, 0), (0, -1), (-1, 0), (0, 1)]

        decision = head $
        	(	filter (/= (-cdx, -cdy)) -- can't go in opposite way
            	$ filter ghostCanMove 
            	$ desiredDirections ++ backupDirections
            ) ++ [(-cdx, -cdy)]
      in (intention ^= decision) ghost
  in
    (player ^%= moveCreature)
    .(food ^%= eat)
    .(ghosts ^%= map moveCreature) 
    .(ghosts ^%= zipWith ghostAI [0..])
    $ state


moveTimeGame state = 
	let
		normal s = let currentState = (intention.player ^= (playerIntention ^$ state)) $ head s 
				   in moveGame currentState : s
		rewind s | length s > 2 = tail s
				 | otherwise = s
	in case timeDirection ^$ state of
		Normal -> states ^%= normal $ state
		Rewind -> states ^%= rewind $ state


-- controls
userAction (GL.Char ' ') GL.Down = timeDirection ^= Rewind
userAction (GL.Char ' ') GL.Up = timeDirection ^= Normal

userAction (GL.SpecialKey key) keyState = 
    let updateX = setL (fstLens.playerIntention) 
        updateY = setL (sndLens.playerIntention)
        (update, newVal) = case key of
                            GL.KeyLeft -> (updateX, -1)
                            GL.KeyRight -> (updateX, 1)
                            GL.KeyUp -> (updateY, -1)
                            GL.KeyDown -> (updateY, 1)
                            _ -> (\_ -> id, 0)
    in case keyState of
          GL.Down -> update newVal
          GL.Up   -> update 0        

userAction _ _ = id