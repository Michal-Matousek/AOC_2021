module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace


foldPaper :: [[String]] -> [[Int]] -> (Int,Int) -> ([[Int]], (Int,Int))
foldPaper [] dotPos (lastX,lastY)  = (dotPos, (lastX,lastY))
foldPaper ([ins,coord]:ix) dotPos (lastX,lastY)
    | ins == "fold along y" = foldPaper ix yFold (lastX,coordNum)
    | ins == "fold along x" = foldPaper ix xFold (coordNum,lastY)
    where xFold = [ if x <= coordNum then [x,y] else [ (2*coordNum - x) ,y]  | [x,y] <- dotPos ]
          yFold = [ if y <= coordNum then [x,y] else [ x, (2*coordNum - y)]  | [x,y] <- dotPos ]
          coordNum = read coord

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc25\\input.txt"

    let dotPos = [ map read (splitOn "," l)  | l <- (lines $ contents), l /= []  && head l /= 'f' ] :: [[Int]] -- 6,10
        foldLines = [ splitOn "=" l  | l <- (lines $ contents), l /= []  && head l == 'f' ] -- fold along y=7 -- horizontal

        (foldedDots,(maxX,maxY)) = foldPaper [head foldLines]  dotPos (0,0)
        -- (foldedDots,(maxX,maxY)) = foldPaper foldLines  dotPos (0,0)
        unique = map head (group (sort foldedDots))        
        dotCount = length unique
        result = maxX * maxY - (length unique)
      
    print dotPos
    print unique
    print result
    print maxX
    print maxY
    print dotCount

    


    

    
    

    

    
