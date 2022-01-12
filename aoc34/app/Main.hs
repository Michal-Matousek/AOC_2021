module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace



isInBounds :: (Int, Int) -> ((Int,Int),(Int,Int)) -> Bool
isInBounds (x,y) ((minX,minY),(maxX,maxY)) = x >= minX && x <= maxX && y >= minY && y <= maxY

functionRoot :: Float -> Float -> Float -> Float
functionRoot a b c = if d < 0 then -1.0 else max x y
                        where
                          x = e + sqrt d / (2 * a)
                          y = e - sqrt d / (2 * a)
                          d = b * b - 4 * a * c
                          e = - b / (2 * a)
{-
bestYFor :: Int -> Int -> ((Int,Int),(Int,Int)) -> Int
bestYFor minLX maxLX tgtBound = maximum [ maxLaunchY tgtBound lx | lx <- [minLX..maxLX] ]

maxLaunchY :: ((Int,Int),(Int,Int)) -> Int -> Int
maxLaunchY tgtBound x = [ maxLaunchYwithSteps tgtBound x st | st <- [minSteps..maxSteps]]
    where (minSteps, maxSteps) = solveStepsForX x (head tgtBound)
-}

isValidLnch :: ((Int,Int),(Int,Int)) -> (Int,Int) -> (Int,Int) -> Int
isValidLnch tgtBound@((minX,minY),(maxX,maxY)) (x,y) (dx,dy)
    | x > maxX || y < minY                  = 0
    | isInBounds (x,y) tgtBound == True     = 1
    | otherwise                             = isValidLnch tgtBound (x+dx,y+dy) ( (max (dx-1)  0 ) , dy-1)


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc33\\input.txt" 

    let -- INPUT                
        lineIn = drop 15 (head (lines $ contents))     -- target area: x=211..232, y=-124..-69
        parts1 = splitOn "," lineIn             -- 211..232 ,  y=-124..-69
        lstIn = (splitOn ".." (head parts1)) ++ (splitOn ".." (drop 3 (last parts1)))
        target@[xMin,xMax,yMin,yMax] = map read lstIn :: [Int]
        tgtBound = ((xMin,yMin),(xMax,yMax))
        
        count1step = ((abs (xMin-xMax))+1)*((abs (yMin-yMax))+1) --- 1 step 

        cKoef = (fromIntegral  xMin)*(-2.0)
        minLX = ceiling   (functionRoot 1.0 1.0 cKoef)
        maxLX = div xMax 2

        minLY = div (yMin+1) 2
        maxLY = (abs yMin) - 1
    
        count2plusSteps = sum [ isValidLnch tgtBound (0,0) (x,y) | x <- [minLX..maxLX] , y<-[minLY..maxLY] ]

        result = count1step + count2plusSteps


    print "---------------------------------------------"            
    print count1step
    print count2plusSteps
    print result
    



    

    


    

    
    

    

    
