module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Ord
import Debug.Trace


countRiskLevel :: [String] -> Int
countRiskLevel (l1:l2:[]) = 0
countRiskLevel (l1:l2:l3:ls) = riskLevel + countRiskLevel (l2:l3:ls)
    where riskLevel = sum [ read ([l2!!i]) + 1  | i<-[1..((length l2)-2)], l2!!i < l2!!(i+1), l2!!i < l2!!(i-1), l2!!i<l1!!i, l2!!i<l3!!i]


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc17\\input.txt"    

    let lineIn = [ ":" ++ l ++ ":" | l <- (lines $ contents)]
        boundLine = take (length (head lineIn)) (repeat ':')
        boundedPlane = [boundLine] ++ lineIn ++ [ boundLine ]

        riskLevel = countRiskLevel boundedPlane


    print riskLevel

    

    
