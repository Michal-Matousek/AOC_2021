module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace


parseLine :: String -> (Int, (Int, Int), (Int,Int), (Int,Int))
parseLine ln = (onOff, (xMin, xMax), (yMin,yMax), (zMin, zMax))
    where [onOffStr, coords] = splitOn " " ln
          onOff = if onOffStr == "on" then 1 else 0
          [xStr, yStr, zStr] = splitOn "," coords
          (xMin, xMax) = coordFromStr xStr
          (yMin, yMax) = coordFromStr yStr
          (zMin, zMax) = coordFromStr zStr

coordFromStr :: String -> (Int,Int)
coordFromStr coord = (a,b)
    where numPart = drop 2 coord
          [aIn,bIn] = map read (splitOn ".." numPart) :: [Int]
          (a,b) = if (aIn <= bIn) then (aIn,bIn) else (bIn,aIn)

listUpdatesLim50 :: (Int, (Int, Int), (Int,Int), (Int,Int)) -> [((Int,Int,Int),Int)]
listUpdatesLim50 (onOff, (xMin, xMax), (yMin,yMax), (zMin, zMax)) = lstChanges
    where (lxMin, lxMax) = (max xMin (-50), min xMax ( 50))
          (lyMin, lyMax) = (max yMin (-50), min yMax ( 50))
          (lzMin, lzMax) = (max zMin (-50), min zMax ( 50))
          lstChanges = [ ((x,y,z),onOff) | x <- [lxMin..lxMax], y <- [lyMin..lyMax], z <- [lzMin..lzMax]]

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc43\\input.txt" 

    let -- INPUT                
        seqList = [ parseLine ln | ln <- (lines $ contents), ln /= "" ]

        core50 = listArray ((-50,-50,-50), (50,50,50)) [0| x<-[(-50)..50], y<-[(-50)..50], z<-[(-50)..50]] 

        coreBoot = core50 // concat ([ listUpdatesLim50 bootStep  | bootStep <- seqList ])
      
        onCubes = sum [ (coreBoot!(x,y,z)) | x<-[(-50)..50], y<-[(-50)..50], z<-[(-50)..50]] 
        

    print "---------------------------------------------"                    
    print onCubes
    
   
    




    

    


    

    
    

    

    
