module Main where

import System.IO

positionSub :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
positionSub [] (x,y, aim) = (x,y, aim)
positionSub ("forward":step:xs) (x,y,aim) = positionSub xs (x + read step, y + aim * read step , aim)
positionSub ("down":step:xs) (x,y,aim) = positionSub xs (x , y , aim + read step)
positionSub ("up":step:xs) (x,y,aim) = positionSub xs (x , y , aim - read step)

mulPairFromTriplet :: (Int, Int, Int) -> Int
mulPairFromTriplet (x,y,z) = x * y


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc03\\input.txt"    

    let inputList = words $ contents 
        resultPair = positionSub inputList (0,0,0)
        result = mulPairFromTriplet resultPair
        strRes = show resultPair ++ " -> " ++ show result

    print strRes
    
