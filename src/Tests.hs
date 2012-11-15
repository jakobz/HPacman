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

segmentsCases = [([(0,0),(2,2),(2,0),(0,2)],Just(1,1)),
			     ([(0,0),(0,2),(2,2),(2,0)],Nothing),
			     ([(1,-2),(1,2),(0,0),(2,0)],Just(1,0))
			    ]

runSegmentTest (params,result) = TestCase 
    $ assertEqual ("Lines intersects " ++ show params) result 
    $ let [c1,c2,c3,c4] = params
      in segmentsIntersects (c1,c2) (c3,c4)

segmentShuffles ([c1,c2,c3,c4], result) = [
				([c1,c2,c3,c4],result),([c2,c1,c3,c4],result),([c1,c2,c4,c3],result),([c2,c1,c4,c3],result),
				([c3,c4,c1,c2],result),([c3,c4,c2,c1],result),([c4,c3,c1,c2],result),([c4,c3,c2,c1],result)
			]

segmentsTests = map runSegmentTest $ concatMap segmentShuffles segmentsCases

runTests =  runTestTT $ TestList $ concat [boxTests, segmentsTests]