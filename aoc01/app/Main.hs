module Main where

increaseCount :: [Int] -> Int 
increaseCount [] = 0
increaseCount (x:[]) = 0
increaseCount (x:y:xs) = (if x<y then 1 else 0) + increaseCount (y:xs)

readInt :: String -> Int
readInt = read

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc01\\input.txt"    

    let inputList = map readInt . words $ contents 
        result = increaseCount inputList

    print result
