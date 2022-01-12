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

searchRoute :: [(Int, Int)] -> Array (Int,Int) Int ->  Array (Int,Int) Int -> Array (Int,Int) Int
searchRoute []  cave routes = routes
searchRoute (pos@(x,y):px) cave routes = {- trace (show pos ++ " " ++ show (routes!pos)) -} (searchRoute updatedSerachList cave updatedRoutes)
    where caveBounds = bounds routes
          maxInt = maxBound  :: Int                    
          
          searchLoc = [ (x+1, y), (x-1,y), (x,y-1), (x,y+1) ]
          posInBound = [ p | p<-searchLoc, True == isInBounds p caveBounds]
          betterPos = [ (p,  (routes!(x,y) + cave!p)) | p <- posInBound, (routes!(x,y) + cave!p) < (routes!p) ]
          updatedRoutes = routes // betterPos
          updatedSerachList = px ++ [ p | (p,_)<-betterPos]          


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc29\\input.txt"

    let -- INPUT        
        template =  (lines $ contents)
        lineIn = [ [ read [c] | c<-l ] | l<-(lines $ contents) ] :: [[Int]]
                
        arraySize = (  ((length  lineIn)-1), (length (head lineIn)) -1 ) -- row, col
        cave = listArray ((0,0),arraySize) (concat lineIn)

        caveBounds = bounds cave
        maxInt = maxBound  :: Int
        searchPos = [(0,0)]
        initialRoutes = (accumArray (+) maxInt caveBounds []  ) // [((0,0), 0)]

        bestRoute = searchRoute searchPos cave initialRoutes
        result = bestRoute!arraySize


    print "---------------------------------------------"    
    --print bestRoute
    print result
    

    


    

    
    

    

    
