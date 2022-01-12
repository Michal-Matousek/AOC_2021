module Main where

import System.IO
import Data.List.Split
import Data.List
import Debug.Trace


fuelFunc :: Int -> Int
fuelFunc n = div (n*(n+1)) 2 


findBetter :: [Int] -> Int -> Int -> (Int, Int)
findBetter crabList aPos aFuel = (bestFuel, bestPos)
    where leftFuel = sum [ fuelFunc (abs (x-aPos-1))  | x <- crabList ]
          rightFuel = sum [ fuelFunc (abs (x-aPos+1))  | x <- crabList ]
          (bestFuel, bestPos) = if (leftFuel >= aFuel && rightFuel >= aFuel) 
                                    then (aFuel, aPos)
                                    else if (leftFuel<rightFuel)
                                         then findBetter crabList (aPos+1) leftFuel
                                         else findBetter crabList (aPos-1) rightFuel
          


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc14\\input.txt"    

    let inNum = map read (splitOn "," (contents))  :: [Int]         
        sortedList = sort inNum
        posInList = div (length inNum) 2
        allignTo = sortedList !! posInList    
        fuel = sum [ fuelFunc (abs (x-allignTo))  | x <- sortedList ]

        (bestFuel, bestPos) = findBetter sortedList allignTo fuel
        fStr = show fuel ++ " Pos:" ++ show allignTo
        bStr = show bestFuel ++ " Pos:" ++ show bestPos
        fuelL = sum [ fuelFunc (abs (x-(allignTo-1)))  | x <- sortedList ]
        fuelR = sum [ fuelFunc (abs (x-(allignTo+1)))  | x <- sortedList ]
        
    
    print fuelL
    print fuelR

    print fStr
    print bStr

    
