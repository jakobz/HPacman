module Vectors where

import Data.Fixed

type Coord = (Float, Float) 

(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)

vecLength :: Coord -> Float
vecLength (x, y) = sqrt $ x * x + y * y
scaleVec scale (x, y) = (x * scale, y * scale)

circlesIntersects :: Float -> Coord -> Coord -> Bool
circlesIntersects size c1 c2 = vecLength (c1 .-. c2) <= size
boxesIntersects ((x1, y1), (sx1, sy1)) ((x2,y2), (sx2, sy2)) = 
    let byX = (abs $ x1 - x2) * 2 <= (sx1 + sx2)
        byY = (abs $ y1 - y2) * 2 <= (sy1 + sy2)
    in byX && byY

truncVec :: Coord -> (Int, Int)
truncVec (x,y) = (truncate x, truncate y)

toFloatVec :: (Int, Int) -> Coord
toFloatVec (x,y) = (fromIntegral x, fromIntegral y)

normalizeVec :: (Coord) -> (Coord)
normalizeVec v = scaleVec (1/vecLength v) v

vecNormal :: (Coord) -> (Coord)
vecNormal (x,y) = normalizeVec (-y, x)

vecAngle (x,y) = atan2 y x

rotateVec a (x,y) =
    let sn = sin a
        cs = cos a        
    in (x * cs - y * sn, x * sn + y * cs)

(x1,y1) .*. (x2,y2) = x1 * y2 - y1 * x2

segmentsIntersects :: (Coord, Coord) -> (Coord, Coord) -> Maybe Coord
segmentsIntersects (s1,e1) (s2,e2) = 
    let p = s1
        r = e1 .-. s1
        q = s2
        s = e2 .-. s2
    in segVecIntersects (p,r) (q,s)

segVecIntersects (p,r) (q,s) = 
    case segVecIntersectsDetail (p,r) (q,s) of
        Just (v, a, b) -> Just (v .+. p)
        Nothing -> Nothing

-- algorythm described here: http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
segVecIntersectsDetail (p,r) (q,s) = 
    let rs = r .*. s
    in if (rs == 0) 
        then Nothing 
        else let 
                qp = (q .-. p) 
                t = (qp .*. s) / rs
                u = (qp .*. r) / rs
            in if (0 <= t && t <= 1 && 0 <= u && u <= 1) 
                then Just $ (scaleVec t r, t, u)
                else Nothing

directionToVec 0 = (-1, 0)
directionToVec 1 = ( 0, 1)
directionToVec 2 = ( 1, 0)
directionToVec 3 = ( 0,-1)