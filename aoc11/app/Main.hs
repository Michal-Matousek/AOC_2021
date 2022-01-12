-- 3099
module Main where

import System.IO
import Data.List.Split
import Debug.Trace

fishList :: [Int] -> Int -> Int
fishList [] days = 0
fishList (x:xs) days = restFishPop + fCount * fishPop x days
    where fCount = sum [ 1 | f <- x:xs, f == x ]
          restFish = [ f | f <- xs, f /= x ]
          restFishPop = fishList restFish days

fishPop :: Int -> Int -> Int
fishPop x days
    | x >= days = 1
    | x < days  = fishPop 0 (days - x - 7) + fishPop 0 (days - x - 9)

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc11\\input.txt"    

    let inputList = splitOn "," (init contents)
        inNum = map read inputList :: [Int]
        
        poulation = fishList inNum 80

    print poulation
