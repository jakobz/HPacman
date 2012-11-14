import Loading
import Gameplay
import Rendering
import Engine
import Tests
import Control.Monad
import Test.HUnit

main = do
	testResults <- runTests
	unless (failures testResults == 0) $ error "Tests failed"
	run $ newGame { 
              load = loadGame,
              move = moveGame,
              render = renderGame,
              handleInput = userAction
            }