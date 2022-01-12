module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace
import Algorithm.Search (aStar,pruning)

isInBounds :: (Int, Int) -> ((Int,Int),(Int,Int)) -> Bool
isInBounds (x,y) ((minX,minY),(maxX,maxY)) = x >= minX && x <= maxX && y >= minY && y <= maxY

isInBoundsPred :: ((Int,Int),(Int,Int)) -> (Int, Int) -> Bool
isInBoundsPred ((minX,minY),(maxX,maxY)) (x,y) = not (x >= minX && x <= maxX && y >= minY && y <= maxY)


searchRoute :: [(Int, Int)] -> Array (Int,Int) Int ->  Array (Int,Int) Int -> Array (Int,Int) Int
searchRoute []  cave routes = routes
searchRoute (pos@(x,y):px) cave routes =  {-trace (show pos ++ " " ++ show (routes!pos))-}  (searchRoute updatedSerachList cave updatedRoutes)
    where caveBounds = bounds routes
          maxInt = maxBound  :: Int                    
          
          searchLoc = [ (x+1, y),  (x,y+1) ] -- , (x-1,y),(x,y-1)
          posInBound = [ p | p<-searchLoc, True == isInBounds p caveBounds]
          betterPos = [ (p,  (routes!(x,y) + cave!p)) | p <- posInBound, (routes!(x,y) + cave!p) < (routes!p) && (routes!(x,y) + cave!p) < (routes!(snd caveBounds)) ]
          lowerPos = [ p | (p@(nx,ny),_)<-betterPos , x + y > nx + ny ]
          higherPos = [ p | (p@(nx,ny),_)<-betterPos , x + y <= nx + ny ]
          updatedRoutes = routes // betterPos
          newPX = [ p |p<-px, p /= pos]
          updatedSerachList = lowerPos ++ newPX ++ higherPos


searchDirectRoute :: (Int,Int) -> (Int,Int) ->  Array (Int,Int) Int ->  (Array (Int,Int) Int , [(Int,Int)]) -> (Array (Int,Int) Int , [(Int,Int)])
searchDirectRoute (x,y) (dx,dy) cave (routes, posList)
    | x == dx && y == dy = (routes , posList)
    | otherwise = searchDirectRoute newPos (dx,dy) cave (updatedRoutes, newPos:posList)
    where searchLoc = [(x,y+1),(x+1,y)]
          caveBounds = bounds routes
          posInBound = [ p | p<-searchLoc, True == isInBounds p caveBounds]
          betterPos = [ (p,  (routes!(x,y) + cave!p)) | p <- posInBound ]
          updatedRoutes = routes // betterPos          
          (hPos, hValue) = head betterPos
          (tPos, tValue) = last betterPos
          newPos = if hValue < tValue then hPos else tPos

cpyInc, lnX5 :: [Int] -> [Int]
cpyInc lst = [ (mod i 9)+1 | i<-lst ]
lnX5 lst = lst ++ (cpyInc lst) ++ (cpyInc (cpyInc lst))  ++ (cpyInc (cpyInc (cpyInc lst)))  ++ (cpyInc (cpyInc (cpyInc (cpyInc lst))))

summed0Row :: Array (Int,Int) Int -> Array (Int,Int) Int -> Int -> Int -> Int -> [((Int,Int),Int)] -> [((Int,Int),Int)]
summed0Row cave routes accumVal i yMax ls
    | i == yMax = ((0,i),(accumVal + cave!(0,i))):ls
    | i <  yMax = summed0Row cave routes val (i+1) yMax (((0,i),val):ls)
    where val = accumVal + (cave!(0,i))

summedXRow :: Int -> Array (Int,Int) Int -> Array (Int,Int) Int -> Int -> Int -> Int -> [((Int,Int),Int)] -> [((Int,Int),Int)]
summedXRow x cave routes accumVal i yMax ls
    | i == yMax = ((x,i),bestVal):ls
    | i <  yMax = summedXRow x cave routes bestVal (i+1) yMax (((x,i),bestVal):ls)
    where valLeft = accumVal + (cave!(x,i))
          valUp = (routes!(x-1,i)) + (cave!(x,i))
          bestVal = if valLeft < valUp then valLeft else valUp

findSimple :: Int -> (Int,Int) -> Array (Int,Int) Int ->  Array (Int,Int) Int -> Array (Int,Int) Int
findSimple 0 (xMax,yMax) cave routes = findSimple 1 (xMax,yMax) cave (routes // (summed0Row cave routes 0 1 yMax []) )    
findSimple x (xMax,yMax) cave routes 
    | x == xMax = updatedRoutes
    | x <  xMax = findSimple (x+1) (xMax,yMax) cave updatedRoutes
    where firstVal = (routes!(x-1,0)) + (cave!(x,0))
          fistRouts = routes // [((x,0),firstVal)]
          updatedRoutes = fistRouts // (summedXRow x cave routes firstVal 1 yMax [])
          

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc29\\input.txt"

    let -- INPUT                
        lineInInit = [ [ read [c] | c<-l ] | l<-(lines $ contents) ] :: [[Int]]
        lineIn = map (lnX5) lineInInit
        initValues = lnX5 (concat lineIn)
                      
        arraySize = (  (5*(length  lineIn)-1), (length (head lineIn)) -1 ) -- row, col
        cave = listArray ((0,0),arraySize) initValues

        -- INIT
        caveBounds = bounds cave
        maxInt = maxBound  :: Int        
        initialRoutes = (accumArray (+) maxInt caveBounds []  ) // [((0,0), 0)]        

        -- Direct route
        -- (directRoutes, directPos) = searchDirectRoute (0,0) arraySize cave (initialRoutes, [])

        -- Search
        -- bestRoute = searchRoute [(0,0)] cave (initialRoutes // [(arraySize, directRoutes!arraySize)])

        -- fullDownRoute = greedyRoute
        -- simpleRoutes = findSimple 0 arraySize cave initialRoutes

        -- A* LIBRARY
        aNeighbors (x, y) = [(x, y + 1), (x - 1, y), (x + 1, y), (x, y - 1)]
        aDistEst (x1, y1) (x2, y2) = (abs (y2 - y1) + abs (x2 - x1)) * 1
        aDist (x1, y1) (x2, y2) = cave!(x2,y2)
        aStart = (0, 0)
        aEnd = arraySize
        aIsInBounds = (isInBoundsPred caveBounds)

        aStarResult = aStar (aNeighbors `pruning` aIsInBounds) aDist (aDistEst aEnd) (== aEnd) aStart

        

        -- result = simpleRoutes!arraySize

        --result = bestRoute!arraySize


    print "---------------------------------------------"    
    print arraySize
    -- print bestRoute
    print aStarResult
    

    


    

    
    

    

    
