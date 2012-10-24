{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding ((.), id)
import Engine
import qualified Graphics.UI.GLUT as GL
import Control.Category
import Data.Lens.Lazy
import Data.Lens.Template 
import Data.Array
import Data.List
import Data.Function (on)
import qualified Data.Set as Set

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

readBlock '#' = Wall
readBlock 'p' = Food
readBlock '+' = Drug
readBlock  _  = Space

cellSize = 16
levelW = 41
levelWC = [0..levelW-1] 
levelH = 37
levelHC = [0..levelH-1]

makeCreature x y = Creature (x*cellSize, y*cellSize) (0, 0) (0, 0)

loadGame = do
  levelData <- readFile "data\\level.txt"
  let level = [ ((x,y), readBlock v)
                      | (y, line) <- zip levelHC $ lines levelData
                      , (x, v) <- zip levelWC line]

  return Game { 
    _player = makeCreature 1 1,
    _ghosts = [makeCreature 17 1, makeCreature 1 33, makeCreature 17 25, makeCreature 21 19],
    walls = array ((0,0),(levelW-1, levelH-1)) $ map (\(c, b) -> (c, b == Wall)) level,
    _food = Set.fromList $ map fst $ filter (\(_, b) -> b == Food) level
  }


-- rendering

backSpr = "back"
point = "point"

getDirName (-1, 0) = 'l'
getDirName (1, 0) = 'r'
getDirName (0, -1) = 'u'
getDirName (0, 1) = 'd'
getDirName (0, 0) = 'r'

itemSpr name (x, y) = spr name x y

getMovePhase (x, y) = "23344311" !! (((x + y) `div` 4) `mod` 8)

renderCreature name c = 
  [itemSpr name c, itemSpr name (fstLens ^!-= levelW * cellSize $ c)]

renderPlayer player@(Creature{_coords = coords, _direction = dir}) =
  renderCreature ("pacman_" ++ [getDirName dir, getMovePhase coords]) coords

renderGhost ghost@(Creature{_coords = coords, _direction = dir}) =
  renderCreature ("ghost_" ++ [getDirName dir]) coords

renderGame state =
    [spr backSpr 0 0]
    ++ [spr "point" (x*cellSize) (y*cellSize) | (x, y) <- Set.elems $ food ^$ state]
    ++ renderPlayer (player ^$ state)
    ++ (concatMap renderGhost (ghosts ^$ state))

-- coordinate helpers
wrapCoords (x, y) = (x `mod` (levelW * cellSize), y `mod` (levelH * cellSize))
toLevelCoords (x, y) = ((x `div` cellSize) `mod` levelW, (y `div` cellSize) `mod` levelH)
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)
vecLength (x, y) = sqrt $ fromIntegral $ x * x + y * y

-- gameplay
moveGame state@(Game{walls = walls}) =
  let 
    playerCoords = coords.player ^$ state

    -- collision detection 
    inWall c = walls ! toLevelCoords c
    collisionPoints = [0, cellSize, cellSize * 2, cellSize * 3 - 1]
    boxInWall c = or [inWall $ c .+. (dx, dy) | dx <- collisionPoints, dy <- collisionPoints] 
    canMove c dir = not $ boxInWall (c .+. dir)

    -- physics
    moveCreature creature@(Creature{_coords = coords, _direction = oldDir}) =
        let 
            currentIntention@(keyX, keyY) = intention ^$ creature
            newDir = case 0 of 
                      _| keyX /= 0 && canMove coords (keyX, 0) -> (keyX, 0)
                      _| keyY /= 0 && canMove coords (0, keyY) -> (0, keyY)
                      _| otherwise -> oldDir
            newCoords = if canMove coords newDir
                          then wrapCoords $ coords .+. newDir
                          else coords
        in Creature newCoords newDir currentIntention

    eat = Set.delete ((toLevelCoords $ coords.player ^$ state) .+. (1, 1))

    -- enemy AI
    normalize n | n < 0 = -1
                | n > 0 = 1
                | otherwise = 0
    ghostAI ghostN ghost = 
      let       
        c = coords ^$ ghost
        (right, bottom) = ((levelW - 1) * cellSize, (levelH - 1) * cellSize)
        scatterTarget = [(0, 0), (right, 0), (right, bottom), (0, bottom)] !! ghostN
        (cdx, cdy) = direction ^$ ghost
        target = scatterTarget
        (dx, dy) = target .-. c
        desiredDirections = 
            sortBy (compare `on` vecLength) 
            $ filter (/= (0, 0)) 
            $ [(0, signum dy), (signum dx, 0)]
        allDirections = [(-1, 0), (0, 1), (1, 0), (0, -1)]
        decision = head
            $ filter (/= (-cdx, -cdy)) -- can't go in opposite way
            $ filter (canMove c) 
            $ desiredDirections ++ allDirections
      in (intention ^= decision) ghost
  in
    (player ^%= moveCreature)
    .(food ^%= eat)
    .(ghosts ^%= map moveCreature) 
    .(ghosts ^%= zipWith ghostAI [0..])
    $ state

-- controls

userAction (GL.SpecialKey key) keyState = 
    let updateX = setL (fstLens.intention.player) 
        updateY = setL (sndLens.intention.player)
        (update, newVal) = case key of
                            GL.KeyLeft -> (updateX, -1)
                            GL.KeyRight -> (updateX, 1)
                            GL.KeyUp -> (updateY, -1)
                            GL.KeyDown -> (updateY, 1)
    in case keyState of
          GL.Down -> update newVal
          GL.Up   -> update 0        

userAction _ _ = id

main = run $ newGame { 
              load = loadGame,
              move = moveGame,
              render = renderGame,
              handleInput = userAction
            }


 