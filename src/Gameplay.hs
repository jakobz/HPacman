module Gameplay (moveGame, userAction) where 

import GameState
import Vectors
import Prelude hiding ((.), id)
import Data.Lens.Lazy
import Control.Category
import Data.Array
import qualified Data.Set as Set
import Data.Function (on)
import qualified Graphics.UI.GLUT as GL
import Data.List
import Debug.Trace

-- gameplay
moveWorld state =
  let 
    playerCoords = coords.player ^$ state
    playerDir = direction.player ^$ state
    levelWalls = (walls.level) ^$ state

    -- collisions with walls    
    tryMove c dir = 
        let 
            pathSegments = passVecThruPortal (level ^$ state) c dir
            (outPoint, outVec, (outDirX, outDirY)) = head $ reverse pathSegments
            normDir d = if abs d > 0.1 then signum d else 0
            newDir = (normDir outDirX, normDir outDirY)
            newC = outPoint .+. outVec .+. if (c == outPoint) then (0,0) else newDir

            pointInWall (x,y) | x >=0, y >=0 = levelWalls ! (x,y)
            pointInWall _ = False

            inWall (dx, dy) = let pathSegments = passVecThruPortal (level ^$ state) newC (dx,dy)
                                  (outPoint, outVec, _) = head $ reverse pathSegments
                               in pointInWall $ toLevelCoords (outPoint .+. outVec)

            collisionPoints = [-creatureCenterShift + 0.5, -(cellSize/2), cellSize/2, creatureCenterShift - 0.5]

            boxInWall c = or [inWall $ (dx, dy) | dx <- collisionPoints, dy <- collisionPoints] 
            canMove = not $ boxInWall (c .+. dir)

        in if canMove then Just (newC, newDir) else Nothing

    creaturesCollide = circlesIntersects 40
    collidesWith c = any (creaturesCollide $ c) 
    ghostsCoords = map (coords ^$) $ ghosts ^$ state
    
    getPlayerState Alive | collidesWith playerCoords ghostsCoords = DeathAnimation deathAnimationLength
    getPlayerState Alive | otherwise = Alive
    getPlayerState (DeathAnimation n) | n > 0 = DeathAnimation (n-1)
    getPlayerState (DeathAnimation n) | otherwise = Dead
    getPlayerState Dead = Dead

    newPlayerState = getPlayerState (playerState ^$ state)

    moveCreature creature@(Creature{_coords = oldCoords, _direction = oldDir}) =
        let 
            currentIntention@(keyX, keyY) = intention ^$ creature
            currentTarget = target ^$ creature
            -- try to change direction to one of intended directions
            newDirs = filter (/=(0,0)) $ [(keyX, 0), (0, keyY), oldDir]
            (newCoords, newDir) = firstJust $ (map (tryMove oldCoords) newDirs) ++ [Just (oldCoords, (0,0))]

        in Creature newCoords newDir currentIntention currentTarget

    eat = Set.delete $ toLevelCoords $ playerCoords

    -- enemy AI. Changes intention of the ghost depending on the game state
    ghostAI ghostN ghost = 
      let       
      	-- prepare necessary info
        ghostCoords = coords ^$ ghost
        distToPlayer = vecLength $ playerCoords .-. ghostCoords
        (cdx, cdy) = direction ^$ ghost

        -- detect collision to another ghosts
        otherGhostsCoords = filter (/=ghostCoords) $ ghostsCoords
        willCollideAnother dir = or $ map (creaturesCollide (ghostCoords .+. dir)) otherGhostsCoords

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

        -- the red one moves towards the player directly
        getAttackTarget 0 = playerCoords
        -- the pink one tries to get in front of the player
        getAttackTarget 1 = playerCoords .+. scaleVec (12 * cellSize) (direction.player ^$ state) 
        -- the blue one tries to get to the back of the player
        getAttackTarget 2 = playerCoords .-. scaleVec (12 * cellSize) (direction.player ^$ state) 
        -- the yellow one tries to move at some distance around the player
        getAttackTarget 3 = if distToPlayer > (16 * 18) then playerCoords else ghostCoords

        attackTarget = getAttackTarget ghostN

        (dx, dy) = attackTarget .-. ghostCoords

        -- 1st and 2nt priorities are to move towards the target by x or y 
        desiredDirections =            
            map (\(x,y) -> (signum x, signum y)) 
            $ reverse
            $ sortBy (compare `on` vecLength) 
            $ filter (/= (0, 0)) 
            $ [(0, dy), (dx, 0)]

        backupDirections = 
            -- 3nd priority is to move forward
            (cdx, cdy) : 
            -- 4nd try left, right, down and up 
            (take 4 $ drop ghostN $ cycle [(1, 0), (0, -1), (-1, 0), (0, 1)])

        directionsByPriority = 
            (filter (\dir -> dir /= (-cdx, -cdy) && not (willCollideAnother dir)) (desiredDirections ++ backupDirections))
            ++ [(-cdx, -cdy), (0,0)] -- the opposite way and stop should always be the last priories

        (newCoords, decision) = firstJust $ map (tryMove ghostCoords) directionsByPriority

      in ((intention ^= decision) . (target ^= attackTarget)) ghost

    newPState = (playerState ^= newPlayerState) $ state   	

    movedState = 
	    (player ^%= moveCreature)
	    .(food ^%= eat)
	    .(ghosts ^%= map moveCreature) 
	    .(ghosts ^%= zipWith ghostAI [0..])
	    $ newPState  	
  in if (newPlayerState == Alive) then movedState else newPState


moveGame state = 
	let
		stateList = worldStates ^$ state
		currentState = (intention.player ^= (playerIntention ^$ state)) $ head stateList 
		isDead = (playerState ^$ nextState) == Dead
		nextState = moveWorld currentState
		rewindedStates = if (length stateList > 2) then tail stateList else stateList
	in case (timeDirection ^$ state, isDead) of
		(Normal, False) -> worldStates ^= nextState : stateList $ state
		(Normal, True) -> (worldStates ^= stateList) $ state
		(Rewind, _) -> worldStates ^= rewindedStates $ state

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