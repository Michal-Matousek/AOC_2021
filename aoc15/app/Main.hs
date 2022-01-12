module Main where

import System.IO
import Data.List.Split
import Data.List
import Debug.Trace


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc15\\input.txt"    

    let lineIn = [splitOn "|" l | l <- (lines $ contents)]                    
        words1478 = concat [ (words l) | [_, l] <- lineIn ] 
        count1478 = sum [1 | w<-words1478, length(w) == 2 || length(w) == 4 || length(w) == 3|| length(w) == 7  ]  

    print words1478
    print count1478

    
