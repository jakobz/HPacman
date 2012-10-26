import GameState
import Gameplay
import Rendering
import Engine

main = run $ newGame { 
              load = loadGame,
              move = moveTimeGame,
              render = renderGame,
              handleInput = userAction
            }