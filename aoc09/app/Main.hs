-- 3099
module Main where

import System.IO
import Data.List.Split
import  Data.Array
import  Data.Ix
import Debug.Trace

lineToPoints :: String -> ((Int,Int),(Int,Int))
lineToPoints str = ( (read x1 , read y1), (read x2, read y2 )) 
    where parts = words str -- ["x1,y1","->","x2,y2"]
          [x1,y1] = splitOn "," (head parts) 
          [x2,y2] = splitOn "," (last parts)

-- from line segment provide list of points
segToPoints :: ((Int,Int),(Int,Int)) -> [((Int,Int),Int)] 
segToPoints ((x1,y1),(x2,y2)) = segPoints
    where segPoints = [ (( x1+i*dx , y1+i*dy ),1) | i<- [0..stepsCount] ]
          dx = if x2-x1 == 0 then 0 else div (x2-x1) (abs (x2-x1))
          dy = if y2-y1 == 0 then 0 else div (y2-y1) (abs (y2-y1)) 
          stepsCount = max (abs (x2-x1)) (abs (y2-y1))

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc09\\input.txt"    

    let inputList = lines $ contents         
        lineSegments =  map (lineToPoints) inputList        
        ortogonalSegments = [ ((x1,y1),(x2,y2)) | ((x1,y1),(x2,y2)) <- lineSegments, x1 == x2 || y1 == y2]        
        ventBuff = accumArray (+) 0 ((0,0),(1000,1000)) ( concat [ segToPoints seg | seg<-ortogonalSegments ])        
        intesections = sum [ 1 | v <- (elems ventBuff), v>=2 ]
    
    
    print intesections
