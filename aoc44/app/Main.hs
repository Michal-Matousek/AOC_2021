module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace

-- OK
parseLine :: String -> (Int, (Int, Int), (Int,Int), (Int,Int))
parseLine ln = (onOff, (xMin, xMax), (yMin,yMax), (zMin, zMax))
    where [onOffStr, coords] = splitOn " " ln
          onOff = if onOffStr == "on" then 1 else 0
          [xStr, yStr, zStr] = splitOn "," coords
          (xMin, xMax) = coordFromStr xStr
          (yMin, yMax) = coordFromStr yStr
          (zMin, zMax) = coordFromStr zStr

-- OK
coordFromStr :: String -> (Int,Int)
coordFromStr coord = (a,b)
    where numPart = drop 2 coord
          [aIn,bIn] = map read (splitOn ".." numPart) :: [Int]
          (a,b) = if (aIn <= bIn) then (aIn,bIn) else (bIn,aIn)

splitToDisjunctOnCubes :: [(Int, (Int, Int), (Int,Int), (Int,Int))] -> [(Int, (Int, Int), (Int,Int), (Int,Int))]
splitToDisjunctOnCubes      [] = []
splitToDisjunctOnCubes  (c1:cx) 
    | cubesAfterSplit == 0                               = splitToDisjunctOnCubes cx -- covered by other cube
    | cubesAfterSplit  > 0 && intersectFound == False    = (head cubeList):(splitToDisjunctOnCubes cx) -- single uncovered On cube
    | cubesAfterSplit  > 0 && intersectFound == True     = splitToDisjunctOnCubes (cubeList++cx)   -- split to multiple cubes -> process individually
    where (intersectFound, cubeList) = splitByFirstCube c1 cx 
          cubesAfterSplit = length cubeList

splitByFirstCube :: (Int, (Int, Int), (Int,Int), (Int,Int)) -> [(Int, (Int, Int), (Int,Int), (Int,Int))] -> (Bool, [(Int, (Int, Int), (Int,Int), (Int,Int))])
splitByFirstCube ( 0, intX, intY, intZ )     cx     = (False, [])
splitByFirstCube ( 1, intX, intY, intZ ) []         = (False, [(1, intX, intY, intZ)])
splitByFirstCube ( 1, intX, intY, intZ ) (c:cx) 
    | cubeIntFound == True                         = (True, intersectionList)
    | cubeIntFound == False                        = splitByFirstCube ( 1, intX, intY, intZ ) cx
    where (cubeIntFound, intersectionList) = cubeIntersect ( 1, intX, intY, intZ ) c 

cubeIntersect :: (Int, (Int, Int), (Int,Int), (Int,Int)) -> (Int, (Int, Int), (Int,Int), (Int,Int)) -> (Bool, [(Int, (Int, Int), (Int,Int), (Int,Int))])
cubeIntersect ( _ , (ax,bx), (ay,by), (az,bz) ) ( onOff , (cx,dx), (cy,dy), (cz,dz) ) = (isIntersect, intersectList)
    where (isSubX, xSeg) = subtractIntervals (ax,bx) (cx,dx)
          (isSubY, ySeg) = subtractIntervals (ay,by) (cy,dy)
          (isSubZ, zSeg) = subtractIntervals (az,bz) (cz,dz)
          isIntersect = if isSubX && isSubY && isSubZ then True else False
          intersectList = if isIntersect == False then [(1, (ax,bx), (ay,by), (az,bz))] 
                                                  else [(1, xs,ys,zs) | (xs,xsIncl) <- xSeg, (ys,ysIncl) <- ySeg, (zs,zsIncl) <- zSeg, xsIncl || ysIncl || zsIncl == True] 

subtractIntervals :: (Int, Int) -> (Int, Int) -> (Bool , [((Int, Int), Bool)])
subtractIntervals (a,b) (c,d) = (isIntersect, segAC ++ segCD ++ segDB)
    where intAC = (a, min (c-1) b)
          intCD = (max c a, min d b)
          intDB = (max a (d+1) , b)
          isIntersect =  (intSize intCD) > 0
          segAC = if (intSize intAC) > 0 then [((intAC), True)]  else []
          segCD = if (intSize intCD) > 0 then [((intCD), False)] else []
          segDB = if (intSize intDB) > 0 then [((intDB), True)]  else []

-- OK
intSize :: (Int,Int) -> Int
intSize (a,b) = b-a+1

-- OK
sumCubes :: [(Int, (Int, Int), (Int,Int), (Int,Int))] -> Int
sumCubes [] =  0
sumCubes ((_, (xMin, xMax), (yMin,yMax), (zMin, zMax)):cs) = cubeSize + sumCubes cs
    where cubeSize = (max 0 (xMax-xMin+1)) * (max 0 (yMax-yMin+1)) * (max 0 (zMax-zMin+1))

printCubes cubes = (intercalate  "\n" [ (show c) | c <- cubes ])

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc44\\input.txt" 

    let -- INPUT                
        inLines = (lines $ contents)
        seqList = [ parseLine ln | ln <- inLines , ln /= "" ]

        onCubeList = splitToDisjunctOnCubes seqList
        cubeSum = sumCubes onCubeList
        
        st = printCubes onCubeList
     

    print "---------------------------------------------"                        
    putStrLn st
    print "---"
    print cubeSum   
   
   
    




    

    


    

    
    

    

    
