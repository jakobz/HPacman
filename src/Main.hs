import GameState
import Gameplay
import Rendering
import Engine

main = run $ newGame { 
              load = loadGame,
              move = moveGame,
              render = renderGame,
              handleInput = userAction
            }