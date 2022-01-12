module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Array.MArray
import Debug.Trace

flashCycle :: Int -> Array (Int,Int) Int -> Int
flashCycle 0 octo = 0
flashCycle i octo = flashCount + flashCycle (i-1) updOcto
    where ((rMin0,cMin0),(rMax0,cMax0)) = bounds octo
          ((rMin,cMin),(rMax,cMax)) = ((rMin0+1,cMin0+1),(rMax0-1,cMax0-1))
          incOcto = octo // [ ((r,c), octo!(r,c) + 1 )  | r<-[rMin..rMax], c<-[cMin..cMax] ];          
          (updOcto, flashCount) = flashAll 1 1 rMax cMax (incOcto, 0)

flashAll :: Int -> Int -> Int -> Int -> (Array (Int,Int) Int, Int) -> (Array (Int,Int) Int, Int)
flashAll r c rMax cMax (octo , flashCount) 
    | flashThis == False && isLast == False = flashAll nextR nextC rMax cMax (octo, flashCount)
    | flashThis == False && isLast == True  = (octo, flashCount)
    | flashThis == True  && isLast == False = flashAll nextR nextC rMax cMax (flashedOcto, newFleshCount)
    | flashThis == True  && isLast == True  = (flashedOcto, newFleshCount)
        where isLast = r == rMax && c == cMax
              nextR = if c==cMax then r+1 else r
              nextC = if c==cMax then 1 else c+1
              flashThis = octo!(r,c) > 9
              (flashedOcto, newFleshCount) = flashOne r c rMax cMax (octo , flashCount)

flashOne :: Int -> Int -> Int -> Int -> (Array (Int,Int) Int, Int) -> (Array (Int,Int) Int, Int)
flashOne r c rMax cMax (octo , flashCount) 
    | inBound == False || octo!(r,c) <= 9 = (octo , flashCount)
    | inBound == True && octo!(r,c) > 9 = flashedNeighbors
        where inBound = r>=1 && c >=1 && r<=rMax && c<=cMax
              flashedOcto = octo // ([ ((ri,ci),octo!(ri,ci)+1) | ri <- [r-1..r+1], ci<-[c-1..c+1], octo!(ri,ci) /= 0 ] ++ [((r,c),0)])
              flashedNeighbors = flashOne (r-1) (c-1) rMax cMax . flashOne (r-1) c rMax cMax . flashOne (r-1) (c+1) rMax cMax .
                                 flashOne (r  ) (c-1) rMax cMax .                              flashOne (r  ) (c+1) rMax cMax .
                                 flashOne (r+1) (c-1) rMax cMax . flashOne (r+1) c rMax cMax $ flashOne (r+1) (c+1) rMax cMax (flashedOcto, flashCount+1)
              
main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc21\\input.txt"    

    let lineIn = [ [0] ++ (map read (map (:[]) l)) ++ [0]  | l <- (lines $ contents) ] :: [[Int]]
        
        boundLine = take (length (head lineIn)) (repeat 0)
        boundedPlane = [boundLine] ++ lineIn ++ [ boundLine ]

        arraySize = (  ((length  boundedPlane)-1), (length boundLine)-1 ) -- row, col
        octo = listArray ((0,0),arraySize) (concat boundedPlane)

        flashes = flashCycle 100 octo
      
    
    print flashes
    


    

    
    

    

    
