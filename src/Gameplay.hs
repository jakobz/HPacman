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

--    creaturesCollide (x1,y1) (x2,y2) = (abs (x1 - x2) <= creatureSize) && (abs (y1 - y2) <= creatureSize)
    creaturesCollide v1 v2 = vecLength (v1 .-. v2) <= 40

    collidesWith c = any (creaturesCollide $ c) 
    ghostsCoords = map (coords ^$) $ ghosts ^$ state
    
    getPlayerState Alive | collidesWith playerCoords ghostsCoords = DeathAnimation deathAnimationLength
    getPlayerState Alive | otherwise = Alive
    getPlayerState (DeathAnimation n) | n > 0 = DeathAnimation (n-1)
    getPlayerState (DeathAnimation n) | otherwise = Dead
    getPlayerState Dead = Dead

    newPlayerState = getPlayerState (playerState ^$ state)

    -- physics
    moveCreature creature@(Creature{_coords = coords, _direction = oldDir}) =
        let 
            currentIntention@(keyX, keyY) = intention ^$ creature
            currentTarget = target ^$ creature
            -- try to change direction to one of intended directions
            newDir = case 0 of 
                      _| keyX /= 0 && canMove coords (keyX, 0) -> (keyX, 0)
                      _| keyY /= 0 && canMove coords (0, keyY) -> (0, keyY)
                      _| otherwise -> oldDir
            -- move along the current direction if it is possible
            newCoords = if canMove coords newDir
                          then wrapCoords $ coords .+. newDir
                          else coords
        in Creature newCoords newDir currentIntention currentTarget

    eat = Set.delete (toLevelCoords $ playerCoords .+. ((cellSize * 3) `div` 2, (cellSize * 3) `div` 2))

    -- enemy AI. Changes intention of the ghost depending on the game state
    ghostAI ghostN ghost = 
      let       
      	-- prepare necessary info
        ghostCoords = coords ^$ ghost
        distToPlayer = vecLength $ playerCoords .-. ghostCoords
        (cdx, cdy) = direction ^$ ghost

        -- detect collision to another ghosts
        otherGhostsCoords = filter (/=ghostCoords) $ ghostsCoords
        ghostCanMove dir = (canMove ghostCoords dir) 
        	&& (not $ collidesWith (ghostCoords .+. dir) otherGhostsCoords)

        -- there are two modes: 
        -- - idle - ghosts move to corners 
        -- - attack - ghosts try to catch the player
        -- each ghost have its own target in both modes
        
        (right, bottom) = ((levelW - 1) * cellSize, (levelH - 1) * cellSize)
        getIdleTarget 0 = (0, 0)
        getIdleTarget 1 = (right, 0)
        getIdleTarget 2 = (right, bottom)
        getIdleTarget 3 = (0, bottom)
        
        idleTarget = getIdleTarget ghostN

        -- the red one attacks the player directly
        getAttackTarget 0 = playerCoords
        -- the pink one tries to get in front of the player
        getAttackTarget 1 = playerCoords .+. scaleVec (direction.player ^$ state) (12 * cellSize)
        -- the blue one tries to get to the back of the player
        getAttackTarget 2 = playerCoords .-. scaleVec (direction.player ^$ state) (12 * cellSize)
        -- the yellow one tries to move at some distance around the player
        getAttackTarget 3 = if distToPlayer > (16 * 18) then playerCoords else ghostCoords

        attackTarget = getAttackTarget ghostN

        (dx, dy) = attackTarget .-. ghostCoords
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
      in ((intention ^= decision) . (target ^= attackTarget)) ghost

    newPState = (playerState ^= newPlayerState) $ state   	

    movedState = 
	    (player ^%= moveCreature)
	    .(food ^%= eat)
	    .(ghosts ^%= map moveCreature) 
	    .(ghosts ^%= zipWith ghostAI [0..])
	    $ newPState  	
  in if (newPlayerState == Alive) then movedState else newPState


moveTimeGame state = 
	let
		stateList = states ^$ state
		currentState = (intention.player ^= (playerIntention ^$ state)) $ head stateList 
		isDead = (playerState ^$ nextState) == Dead
		nextState = moveGame currentState
		rewindedStates = if (length stateList > 2) then tail stateList else stateList
	in case (timeDirection ^$ state, isDead) of
		(Normal, False) -> states ^= nextState : stateList $ state
		(Normal, True) -> (states ^= stateList) $ state
		(Rewind, _) -> states ^= rewindedStates $ state

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