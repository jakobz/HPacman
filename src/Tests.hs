module Tests(runTests) where 

import Test.HUnit
import GameState
import Control.Monad

boxCases = [(((0,0),(3,3)),((1,1),(1,1)),True), 
            (((0,0),(1,1)),((1,1),(1,1)),True),
            (((0,0),(1,1)),((2,2),(1,1)),False),
            (((0,0),(2,2)),((1,1),(2,2)),True),
            (((0,0),(2,2)),((4,4),(2,2)),False)]

runBoxTest (c1,c2,result) = TestCase 
    $ assertEqual ("Box intersects " ++ show c1 ++ show c2) result 
    $ boxesIntersects c1 c2

boxTests = map runBoxTest
           $ concatMap (\(c1,c2,result) -> [(c1,c2,result),(c2,c1,result)]) boxCases

runTests =  runTestTT $ TestList $ concat [boxTests]