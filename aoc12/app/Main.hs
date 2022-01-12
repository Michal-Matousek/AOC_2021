module Main where

import System.IO
import Data.List.Split
import Debug.Trace

doIterations :: Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int, Int, Int, Int)
doIterations 0 (a0,a1,a2,a3,a4,a5,a6,a7,a8) = (a0,a1,a2,a3,a4,a5,a6,a7,a8)
doIterations i (a0,a1,a2,a3,a4,a5,a6,a7,a8) = doIterations (i-1) (a1,a2,a3,a4,a5,a6,a7+a0,a8,a0)

sumVector :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Int
sumVector (a0,a1,a2,a3,a4,a5,a6,a7,a8) = a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8

countNum :: [String] -> String -> Int
countNum list numStr = sum [ 1 | l<-list, l==numStr ] 

f(n) = 2 ^ (div n 7)  - 2 ^ ((div n 7) - 1) + (div n 7) 


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc11\\input.txt"    

    let inNum = splitOn "," (init contents)              
        
        vecInit = (countNum inNum "0",countNum inNum "1",countNum inNum "2",countNum inNum "3",countNum inNum "4",countNum inNum "5",countNum inNum "6",0,0)
        vec0 = doIterations 0 (0,0,0,0,0,0,1,0,0)        
        vec10 = doIterations 10 (0,0,0,0,0,0,1,0,0)
        f10 = f 10
        tot10 = sumVector vec10
        vec30 = doIterations 40 (0,0,0,0,0,0,1,0,0)
        f30 = f 40
        tot30 = sumVector vec30

        -- total = sumVector vec
    
    -- print vecInit
    putStrLn ("  1" ++ show vec0)
    putStrLn (" 10" ++ show vec10 ++ " tot:" ++ show tot10 ++ " f:" ++ show f10)
    putStrLn (" 40" ++ show vec30 ++ " tot:" ++ show tot30 ++ " f:" ++ show f30)
        
