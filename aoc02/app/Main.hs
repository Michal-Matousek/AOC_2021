module Main where
    
increaseCount :: [Int] -> Int 
increaseCount [] = 0
increaseCount (x1:[]) = 0
increaseCount (x1:x2:[]) = 0
increaseCount (x1:x2:x3:[]) = 0
increaseCount (x1:x2:x3:x4:xs) = (if (x1<x4) then 1 else 0) + increaseCount (x2:x3:x4:xs)

readInt :: String -> Int
readInt = read

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc01\\input.txt"    

    let inputList = map readInt . words $ contents 
        result = increaseCount inputList

    print result
