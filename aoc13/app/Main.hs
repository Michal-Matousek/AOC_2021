module Main where

import System.IO
import Data.List.Split
import Data.List
import Debug.Trace





main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc13\\input.txt"    

    let inNum = map read (splitOn "," (init contents)) :: [Int]
        sortedList = sort inNum
        posInList = div (length inNum) 2
        allignTo = sortedList !! posInList    
        fuel = sum [ abs (x-allignTo)  | x <- sortedList ]

        
        
    print fuel
    
