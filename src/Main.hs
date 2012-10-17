{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding ((.), id)
import Engine
import qualified Graphics.UI.GLUT as GL
import Control.Category
import Data.Lens.Lazy
import Data.Lens.Template 
import Data.Array
import Data.List

data Coord = Coord { _x:: Int, _y :: Int } deriving (Eq, Show)

data BlockType = Space | Wall | Food | Drug deriving (Eq)

data Game = Game {
                level :: Array (Int, Int) BlockType,
                _player :: Creature,
                _ghosts :: [Creature]
            }

data Creature = Creature {
                coords :: Coord,
                direction :: Coord,
                _intention :: Coord,
                lastDirection :: Coord 
            }

$( makeLenses [''Coord, ''Game, ''Creature] )            

readBlock '#' = Wall
readBlock 'p' = Food
readBlock '+' = Drug
readBlock  _  = Space

cellSize = 16
levelW = 41
levelWC = [0..levelW-1] 
levelH = 37
levelHC = [0..levelH-1]

makeCreature x y = Creature (Coord (x*cellSize) (y*cellSize)) (Coord 0 0) (Coord 0 0) (Coord 1 0)

loadGame = do
  levelData <- readFile "data\\level.txt"
  let level = array ((0,0),(levelW-1, levelH-1))
                     [ ((x,y), readBlock v)
                      | (y, line) <- zip levelHC $ lines levelData
                      , (x, v) <- zip levelWC line]
  return Game { 
    _player = makeCreature 1 1,
    _ghosts = [makeCreature 10 1, makeCreature 1 33],
    level = level
  }


-- rendering

backSpr = "back"
wall = "wall"
point = "point"

getDirName (Coord (-1) 0) = 'l'
getDirName (Coord 1 0) = 'r'
getDirName (Coord 0 (-1)) = 'u'
getDirName (Coord 0 1) = 'd'
getDirName (Coord 0 0) = 'l'

itemSpr name (Coord x y) = spr name x y

getMovePhase (Coord x y) = ['1','2','2','3','4','3','1','1'] !! (((x + y) `div` 4) `mod` 8)

renderCreature name c = 
  [itemSpr name c, itemSpr name (x ^!-= levelW * cellSize $ c)]


renderPlayer player@(Creature{coords = coords, lastDirection = dir}) =
  renderCreature ("pacman_" ++ [getDirName dir, getMovePhase coords]) coords

renderGhost ghost@(Creature{coords = coords, lastDirection = dir}) =
  renderCreature ("ghost_" ++ [getDirName dir]) coords

renderGame state@(Game{level = level}) =
    [spr backSpr 0 0]
    ++ [spr "point" (x*cellSize) (y*cellSize) | x <- levelWC, y <- levelHC, level!(x,y) == Food]
    ++ renderPlayer (player ^$ state)
    ++ (concatMap renderGhost (ghosts ^$ state))

-- coordinates helpers
wrapX x = x `mod` (levelW * cellSize)
wrapY y = y `mod` (levelH * cellSize)
toLevelCoords c = c `div` cellSize
zeroCoord = Coord 0 0
addCoord (Coord x1 y1) (Coord x2 y2) = Coord (x1 + x2) (y1 + y2)

-- gameplay
moveGame state@(Game{level = level}) =
  -- level helpers  
  let 
    -- collision detection 
    inWall x y = level !(toLevelCoords $ wrapX x, toLevelCoords $ wrapY y) == Wall
    checkpoints size = [0,cellSize .. (size - 1)*cellSize] ++ [size * cellSize - 1]
    boxInWall (Coord x y) size level = 
      or [inWall (x + x') (y + y') | x' <- checkpoints size, y' <- checkpoints size] 

    -- physics
    moveCreature level creature@(Creature{coords = coords, direction = oldDir}) =
        let canMove c dir = not $ boxInWall (addCoord c dir) 3 level
            currentIntention@(Coord keyX keyY) = intention ^$ creature
            newDir = case 0 of 
                      _| keyX /= 0 && canMove coords (y ^= 0 $ currentIntention) -> Coord keyX 0
                      _| keyY /= 0 && canMove coords (x ^= 0 $ currentIntention) -> Coord 0 keyY
                      _| canMove coords oldDir -> oldDir
                      _| otherwise -> zeroCoord
            Coord newX newY = addCoord coords newDir
            lastDir = if newDir == zeroCoord then lastDirection creature else newDir
        in Creature (Coord (wrapX newX) (wrapY newY)) newDir currentIntention lastDir    

    -- enemy AI
    ghostAI level = (intention ^= Coord 1 0)
  in
    (player ^%= moveCreature level)
    .(ghosts ^%= map (moveCreature level))
    .(ghosts ^%= map (ghostAI level))
    $ state

-- controls

updateKeysX = setL (x.intention.player) 

updateKeysY = setL (y.intention.player)

userAction (GL.SpecialKey GL.KeyLeft) GL.Down = updateKeysX (-1)
userAction (GL.SpecialKey GL.KeyRight) GL.Down = updateKeysX 1
userAction (GL.SpecialKey GL.KeyLeft) GL.Up = updateKeysX 0
userAction (GL.SpecialKey GL.KeyRight) GL.Up = updateKeysX 0

userAction (GL.SpecialKey GL.KeyUp) GL.Down = updateKeysY (-1)
userAction (GL.SpecialKey GL.KeyDown) GL.Down = updateKeysY 1
userAction (GL.SpecialKey GL.KeyUp) GL.Up = updateKeysY 0
userAction (GL.SpecialKey GL.KeyDown) GL.Up = updateKeysY 0

userAction _ _ = id

main = run $ newGame { 
              load = loadGame,
              move = moveGame,
              render = renderGame,
              handleInput = userAction
            }


 