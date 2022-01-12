module Main where

positionSub :: [String] -> (Int, Int) -> (Int, Int)
positionSub [] (x,y) = (x,y)
positionSub ("forward":step:xs) (x,y) = positionSub xs (x + read step, y)
positionSub ("down":step:xs) (x,y) = positionSub xs (x , y + read step)
positionSub ("up":step:xs) (x,y) = positionSub xs (x , y - read step)

mulPair :: (Int, Int) -> Int
mulPair (x,y) = x * y

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc03\\input.txt"    

    let inputList = words $ contents 
        resultPair = positionSub inputList (0,0)
        result = mulPair resultPair
        strRes = show resultPair ++ " -> " ++ show result

    print strRes
    
