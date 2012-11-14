module Tests(runTests) where 

import Test.HUnit
import GameState
import Control.Monad

sampleTest = TestCase $ assertEqual "True=False" True True

runTests =  runTestTT $ TestList [sampleTest]