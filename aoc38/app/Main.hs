module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace


readScanners :: [String] -> [[(Int,Int,Int)]] -> [[(Int,Int,Int)]] 
readScanners []     scn = scn
readScanners ("":xs) scn = readScanners (xs) scn 
readScanners (x:xs) scn = if (take 3 x) == "---" then readScanners xs ([]:scn)
                                                 else readScanners xs ((coord:crSc):restSc)
    where (crSc:restSc) = scn
          coordsStr@[a,b,c] = splitOn "," x
          coord = (read a, read b, read c) :: (Int,Int,Int)


pointDist :: (Int,Int,Int) -> (Int,Int,Int) -> Int
pointDist (x1,y1,z1) (x2,y2,z2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)

{-
shortestDistOverLimit :: Int -> [[(Int,Int,Int)]] -> (Int, (Int,Int,Int), (Int,Int,Int))
shortestDistOverLimit limDist points = bestDist
    where bestDist = minimum [ (pointDist a b, a, b) | a<-points, b<-points, a /= b, pointDist a b > limDist ]
-}

projectPointList :: (Int,Int,Int, (Int,Int,Int, (Int,Int,Int))) ->  [(Int,Int,Int)] -> [(Int,Int,Int)]
projectPointList (grpId, fromId, toId, (r1,r2,r3, trans)) []    = []
projectPointList (grpId, fromId, toId, (r1,r2,r3, trans)) (p:ps)  = (projectPoint  (r1,r2,r3, trans) p): projectPointList (grpId, fromId, toId, (r1,r2,r3, trans)) ps

projectPoint :: (Int,Int,Int, (Int,Int,Int)) -> (Int,Int,Int) -> (Int,Int,Int)
projectPoint  (r1,r2,r3, trans) (x,y,z) = translatePoint trans rotP 
    where rotP = rotatePoint (r1,r2,r3) (x,y,z)

identityProjection :: (Int, Int, Int, (Int, Int,Int))
identityProjection = (1,2,3,(0,0,0))

translatePoint :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
translatePoint (tx,ty,tz) (x,y,z) = (x+tx,y+ty,z+tz)

rotatePoint :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
rotatePoint (r1,r2,r3) (x,y,z) = (rx,ry,rz)
    where cInd = [0,x,y,z]
          rx = if (r1>0) then cInd!!r1 else (cInd!!(r1*(-1))) * (-1)
          ry = if (r2>0) then cInd!!r2 else (cInd!!(r2*(-1))) * (-1)
          rz = if (r3>0) then cInd!!r3 else (cInd!!(r3*(-1))) * (-1)

getEqualDist :: Int -> [(Int, Int, (Int,Int,Int), (Int,Int,Int))] -> [(Int, Int, (Int,Int,Int), (Int,Int,Int))]
getEqualDist dist [] = []
getEqualDist dist (d:dx)  
    | dist < crDist  = []
    | dist == crDist = (crDist, crScId, crP1, crP2):(getEqualDist dist dx)
    where  (crDist, crScId, crP1, crP2) = d

-- projection form P1 to P2
joinProjections :: (Int,Int,Int, (Int,Int,Int)) -> (Int,Int,Int, (Int,Int,Int)) -> (Int,Int,Int, (Int,Int,Int))
joinProjections (p1r1,p1r2,p1r3, (p1tx, p1ty,p1tz)) (p2r1,p2r2,p2r3, (p2tx, p2ty,p2tz)) = --trace ("Join" ++ (show (p1r1,p1r2,p1r3, (p1tx, p1ty,p1tz))) ++ " + " ++ (show (p2r1,p2r2,p2r3, (p2tx, p2ty,p2tz))) ++ " -> " ++ (show (r1,r2,r3, (tx,ty,tz)))) 
                                                                                          (r1,r2,r3, (tx,ty,tz))
    where (r1,r2,r3) =  (rotatePoint (p2r1,p2r2,p2r3) (p1r1,p1r2,p1r3))
          (t1x,t1y,t1z) = rotatePoint (p2r1,p2r2,p2r3) (p1tx, p1ty,p1tz)
          (tx,ty,tz) = translatePoint (t1x,t1y,t1z) (p2tx, p2ty,p2tz)

mergeMatchingGrp :: (Bool, (Int, Int, (Int,Int,Int), (Int,Int,Int)), (Int, Int, (Int,Int,Int), (Int,Int,Int)), (Int,Int,Int, (Int,Int,Int))) 
                    -> Array Int (Int,Int,Int, (Int,Int,Int, (Int,Int,Int))) -> Array Int (Int,Int,Int, (Int,Int,Int, (Int,Int,Int)))
mergeMatchingGrp (isMatch, d1, d2, pr) scanGrp = updatedScanGrp
    where (d1Dist, d1ScId, d1P1, d1P2) = d1
          (d2Dist, d2ScId, d2P1, d2P2) = d2
          (d1GrpId, d1From, d1To, d1Proj ) = scanGrp!d1ScId
          (d2GrpId, d2From, d2To, d2Proj ) = scanGrp!d2ScId
          scanNum = length scanGrp
          -- all d1GrpId scaners replaced by d2GrpId
          scansForUpdate = [ (scanGrp!i)  | i <- [1..scanNum], (idFromGrp (scanGrp!i)) == d1GrpId ] 
          projFromD1toD2head = joinProjections pr d2Proj
          projFromD1headToD2head = joinProjections (invProj d1Proj) projFromD1toD2head
          updatedScanGrp = scanGrp // [ (gFrom, ( d2GrpId, gFrom, d2To, joinProjections gProj projFromD1headToD2head )) | ( gId, gFrom, gTo, gProj ) <- scansForUpdate ]         

transfromGroupsTo :: Int -> Array Int (Int,Int,Int, (Int,Int,Int, (Int,Int,Int))) -> Array Int (Int,Int,Int, (Int,Int,Int, (Int,Int,Int)))
transfromGroupsTo ind scanGrp = scanGrp // updateList
    where grpCout = length scanGrp
          listToUpdate = [ (i, scanGrp!i) | i <- [1..grpCout] ] 
          (sGrpId, sFrom, sTo, sProj ) = scanGrp!ind
          invProjSelected = invProj sProj
          updateList = [ (i, (ind, fromId, ind, joinProjections pr  invProjSelected) ) | (i, (id,fromId, tId, pr) ) <- listToUpdate]

idFromGrp :: (Int,Int,Int, (Int,Int,Int, (Int,Int,Int))) -> Int
idFromGrp (grpId, _, _, _ ) = grpId

invProj :: (Int,Int,Int, (Int,Int,Int)) -> (Int,Int,Int, (Int,Int,Int))
invProj (r1,r2,r3, trans) = (nr1,nr2,nr3, (-rtx, -rty, -rtz))
    where invR = sort[(abs(r1),1,signum r1), (abs(r2), 2, signum r2), (abs(r3), 3, signum r3) ]
          [nr1,nr2,nr3] = [ r*sg | (pos, r, sg) <-invR]
          (rtx, rty, rtz) = rotatePoint (nr1,nr2,nr3) trans

getMatchAndProjection :: Array Int [(Int,Int,Int)] -> Array Int (Int,Int,Int, (Int,Int,Int, (Int,Int,Int))) 
                    -> (Int, Int, (Int,Int,Int), (Int,Int,Int)) -> (Int, Int, (Int,Int,Int), (Int,Int,Int))
                    -> (Bool, (Int, Int, (Int,Int,Int), (Int,Int,Int)), (Int, Int, (Int,Int,Int), (Int,Int,Int)), (Int,Int,Int, (Int,Int,Int))) 
getMatchAndProjection scanner scanGrp dist1 dist2
    | isMatch == False  = ( isMatch, dist1, dist2, identityProjection )
    | isMatch == True   = ( isMatch, dist1, dist2, matchingProjection )
    where (d1Dist, d1ScId, d1P1, d1P2) = dist1
          (d2Dist, d2ScId, d2P1, d2P2) = dist2
          (d1GrpId, d1From, d1To, d1Proj ) = scanGrp!d1ScId
          (d2GrpId, d2From, d2To, d2Proj ) = scanGrp!d2ScId
          distVectorProjections = getProjByVector d1P1 d1P2 d2P1 d2P2
          verifiedProjections = verifyProjections dist1 dist2 scanner distVectorProjections
          isMatch = (d1GrpId /= d2GrpId) && ( (length distVectorProjections) >= 1 ) && ( (length verifiedProjections) >= 1 )
          matchingProjection = (head verifiedProjections)


getProjByVector :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int) -> [(Int,Int,Int, (Int,Int,Int))]
getProjByVector (ax,ay,az) (bx,by,bz) (cx,cy,cz) (dx,dy,dz) = concat foundProjs
    where [abx, aby, abz] = [bx-ax, by-ay, bz-az]
          [cdx, cdy, cdz] = [dx-cx, dy-cy, dz-cz]
          foundProjs =  [ 
                            [ (r1,r2,r3,                (getTranslation (r1,r2,r3) (ax,ay,az) (cx,cy,cz)) ) ,
                              ((-1)*r1,(-1)*r2,(-1)*r3, (getTranslation ((-1)*r1,(-1)*r2,(-1)*r3) (ax,ay,az) (dx,dy,dz))) ] 
                            | r1 <- [-3..3], r2 <- [-3..3], r3 <- [-3..3], abs r1 /= abs r2, abs r2 /= abs r3, abs r1 /= abs r3, r1 /= 0, r2 /= 0, r3 /= 0, rotatePoint (r1,r2,r3) (abx,aby,abz) == (cdx,cdy,cdz) ]
          
getTranslation :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
getTranslation (r1,r2,r3) (ax,ay,az) (cx,cy,cz) = (cx-rx, cy-ry, cz-rz)
    where (rx,ry,rz) = rotatePoint (r1,r2,r3) (ax,ay,az)

verifyProjections :: (Int, Int, (Int,Int,Int), (Int,Int,Int)) -> (Int, Int, (Int,Int,Int), (Int,Int,Int)) -> Array Int [(Int,Int,Int)] 
                     -> [(Int,Int,Int, (Int,Int,Int))] -> [(Int,Int,Int, (Int,Int,Int))]
verifyProjections dist1 dist2 scanner [] = []
verifyProjections dist1 dist2 scanner (proj:ps)
    | projectionIsValid == False    =  verifyProjections dist1 dist2 scanner ps
    | projectionIsValid == True     =  [proj]  -- first valid projection is enough
    where   (d1Dist, d1ScId, d1P1, d1P2) = dist1
            (d2Dist, d2ScId, d2P1, d2P2) = dist2
            projectedDist1Scan = [ projectPoint proj p | p <- (scanner!d1ScId) ]
            pointsInRange = [ (x,y,z) | (x,y,z) <- projectedDist1Scan, x>=(-1000),x<=1000, y>=(-1000),y<=1000, z>=(-1000),z<=1000]             
            missingPoints = ([ p | p <- pointsInRange, False == (elem p (scanner!d2ScId)) ])
            projectionIsValid = ((length pointsInRange) >= 12) && (length missingPoints) == 0


matchScanGrps :: Array Int [(Int,Int,Int)] -> Int -> [(Int, Int, (Int,Int,Int), (Int,Int,Int))] 
                 -> Array Int (Int,Int,Int, (Int,Int,Int, (Int,Int,Int))) -> Array Int (Int,Int,Int, (Int,Int,Int, (Int,Int,Int)))
matchScanGrps scanner 1 distList scanGrp = scanGrp
matchScanGrps scanner i (dist:distList) scanGrp 
    | sameDistNum < 1                          = matchScanGrps scanner i distList scanGrp
    | matchFound == False                      = matchScanGrps scanner i distList scanGrp
    | matchFound == True                       = --trace ("Grps after meegr:" ++ (show scanGrpMerged)) 
                                                  (matchScanGrps scanner (i-1) distList scanGrpMerged    )
    where  (crDist, crScId, crP1, crP2) = dist 
           initialEqualDist = getEqualDist crDist distList
           sameDistNum = length initialEqualDist
           candidatesEval = [  getMatchAndProjection scanner scanGrp dist d  | d <- initialEqualDist]
           validMatch = [ (isMatch, d1, d2, pr)  | (isMatch, d1, d2, pr) <-candidatesEval, isMatch == True]
           matchFound = ((length validMatch) >= 1)
           scanGrpMerged = --trace ("Grps befor meegr:" ++ (show scanGrp)) 
                           (mergeMatchingGrp (head validMatch) scanGrp)


mDist :: (Int,Int,Int, (Int,Int,Int, (Int,Int,Int))) -> (Int,Int,Int, (Int,Int,Int, (Int,Int,Int))) -> Int
mDist (_, _, _, d1Proj ) (_, _, _, d2Proj ) = abs(x1-x2) + abs(y1-y2) + abs (z1-z2)
    where (_,_,_,(x1,y1,z1)) = d1Proj
          (_,_,_,(x2,y2,z2)) = d2Proj


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc37\\input.txt" 

    let -- INPUT                
        linesIn = (lines $ contents)     -- --- scanner 0 ---
                                         -- -730,602,-747                                         
        scannersIn = reverse (readScanners linesIn []) -- no rev 332
        scanNum = length scannersIn
        scanner = listArray (1, scanNum) scannersIn
        scanGrpInit = listArray (1, scanNum) [ (i, i, i, identityProjection ) | i<-[1..scanNum] ]

        distList = sort [ (pointDist p1 p2, scId, p1, p2) | scId <- [1..scanNum], p1 <- (scanner!scId), p2 <- (scanner!scId), p1<p2 ]

        scanGrp = matchScanGrps scanner scanNum distList scanGrpInit   
        -- trGrp = transfromGroupsTo 1 scanGrp

        --beacons = concat [ projectPointList (scanGrp!i) (scanner!i) | i<-[1..scanNum] ]
        --uniqueBeacons = map head (group(sort beacons))
        --result = length uniqueBeacons

        maxDist = maximum [ mDist (scanGrp!i) (scanGrp!j) | i<-[1..scanNum], j<-[1..scanNum], i<j]
        result = maxDist

        -- DEBUG
        proj1 = projectPointList (scanGrp!1) (scanner!1)
        proj3 = projectPointList (scanGrp!3) (scanner!3)
        sc1 = scanner!1
{-
        test = getMatchAndProjection scanner scanGrpInit (2421,1,(-660,-479,-426),(-627,-443,-432)) (2421,3,(646,-828,498),(682,-795,504))
        vc = getProjByVector (-660,-479,-426) (-627,-443,-432) (646,-828,498) (682,-795,504)
        vfc = verifyProjections (2421,1,(-660,-479,-426),(-627,-443,-432)) (2421,3,(646,-828,498),(682,-795,504)) scanner vc

        t = invProj (2,1,-3,(10,0,1))
        t1 = joinProjections (2,1,-3,(168,-1125,72)) (2,1,-3,(1125,-168,72))

        ap = (2,3,1,(-20,-1133,1061))
        bp = (2,1,-3,(168,-1125,72))
        cp = (2,1,-3,(1125,-168,72))
        bp2ap = joinProjections bp ap
        cp4bp2ap = joinProjections cp bp2ap
        cp2bp = joinProjections cp bp
        cp2bp4ap = joinProjections cp2bp ap
-}

        ap = (2,3,1,(1086,1177,-45))
        invAp = invProj ap
        s = joinProjections ap invAp

    print "---------------------------------------------"                
    print result
    print scanGrp
    print "---"
    -- print trGrp

--    print ap
  --  print invAp
    --print s



{-
    print "Debug:"                    
    print scanGrp
    print t
    print t1
    print "-"
    print ap
    print bp
    print cp
    print bp2ap
    print cp4bp2ap
    print cp2bp
    print cp2bp4ap
-}

    
-- 461 to high    




    

    


    

    
    

    

    
