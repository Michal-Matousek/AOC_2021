module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Ord
import Debug.Trace

decodeDigit :: [String] -> String -> Int
decodeDigit numList digitStr
    | dLen == 2 = 1
    | dLen == 3 = 7
    | dLen == 4 = 4
    | dLen == 7 = 8
    | dLen == 5 && contains1 == True = 3
    | dLen == 6 && contains1 == False = 6
    | dLen == 6 && contains4 == True = 9
    | dLen == 6 && contains4 == False = 0
    | dLen == 5 && containsBD == True = 5
    | dLen == 5 && containsBD == False = 2
    where dLen = length digitStr 
          contains1 = isSubsequenceOf  (numList!!0) digitStr
          contains4 = isSubsequenceOf  (numList!!2) digitStr
          segBD = [ c| c<-numList!!2, False == elem c (numList!!0) ]
          containsBD = isSubsequenceOf  segBD digitStr

decodeNumber :: [String] -> [String] -> Int
decodeNumber part1 part2 = 1000 * digits!!0 + 100 * digits!!1 + 10 * digits!!2 + digits!!3
    where numList = sortBy (comparing length) part1
          digits = [decodeDigit numList digitStr | digitStr <- part2]

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc15\\input.txt"    

    let lineIn = [splitOn "|" l | l <- (lines $ contents)]                    
        lineList = [ [map sort (words a), map sort (words b)]  | [a, b] <- lineIn ] 
        digitSum = sum [ decodeNumber part1 part2 | [part1,part2] <- lineList]
        

    print digitSum

    

    
