module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Array.MArray
import Debug.Trace

basinSizes :: Array (Int,Int) Char -> [Int]
basinSizes cave = basinSizeList
    where   ((rMin0,cMin0),(rMax0,cMax0)) = bounds cave
            ((rMin,cMin),(rMax,cMax)) = ((rMin0+1,cMin0+1),(rMax0-1,cMax0-1))
            basinSizeList = [ basinSize cave (r,c) | r<-[rMin..rMax], c<-[cMin..cMax], cave!(r,c) < '9', cave!(r,c) < cave!(r+1,c), cave!(r,c) < cave!(r-1,c), cave!(r,c)<cave!(r,c+1), cave!(r,c)<cave!(r,c-1)];

basinSize :: Array (Int,Int) Char -> (Int, Int) -> Int
basinSize cave (row, col) = size
    where (size,_) = countBasinSize (row, col) (0,cave)
    

countBasinSize :: (Int, Int) -> (Int, Array (Int,Int) Char) -> (Int, Array (Int,Int) Char)
countBasinSize (row, col) ( curSize, cave)
    | cave!(row,col) >= '9' = (curSize, cave)
    | cave!(row,col) < '9' = countBasinSize (row+1, col) .
                             countBasinSize (row-1, col) .
                             countBasinSize (row, col+1) $
                             countBasinSize (row, col-1) (curSize+1, updCave)

    where updCave = cave // [((row, col),':')]


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc17\\input.txt"    

    let lineIn = [ ":" ++ l ++ ":" | l <- (lines $ contents)]        
        boundLine = take (length (head lineIn)) (repeat ':')        
        boundedPlane = [boundLine] ++ lineIn ++ [ boundLine ]

        arraySize = (  ((length  boundedPlane)-1), (length boundLine)-1 ) -- row, col
        cave = listArray ((0,0),arraySize) (concat boundedPlane)

        basinList = basinSizes cave
        basinTop3 = take 3 (sortBy (flip compare) basinList)
        result = product basinTop3
        
    print basinList
    print result
    

    

    
