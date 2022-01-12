module Main where

import System.IO

-- first bit in count
-- next 12 bit is 1 bit count 
countBits :: [String] -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
countBits [] (n, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) = (n, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)
countBits (x:xs) (n, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) = (toBitCount x) `sumBitCount` (countBits xs (n, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12))


sumBitCount :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
sumBitCount (an, ab1, ab2, ab3, ab4, ab5, ab6, ab7, ab8, ab9, ab10, ab11, ab12) (bn, bb1, bb2, bb3, bb4, bb5, bb6, bb7, bb8, bb9, bb10, bb11, bb12) = (an + bn, ab1 + bb1, ab2 + bb2, ab3 + bb3, ab4 + bb4, ab5 + bb5, ab6 + bb6, ab7 + bb7, ab8+bb8, ab9+bb9, ab10+bb10, ab11+bb11, ab12+bb12) 

toBitCount :: String -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
toBitCount [] = (0,0,0,0,0,0,0,0,0,0,0,0,0)
toBitCount (s) = (1, if s!!(0)=='1' then 1 else 0, if s!!(1)=='1' then 1 else 0, if s!!(2)=='1' then 1 else 0, if s!!(3)=='1' then 1 else 0, if s!!(4)=='1' then 1 else 0, if s!!(5)=='1' then 1 else 0, if s!!(6)=='1' then 1 else 0, if s!!(7)=='1' then 1 else 0, if s!!(8)=='1' then 1 else 0, if s!!(9)=='1' then 1 else 0, if s!!(10)=='1' then 1 else 0, if s!!(11)=='1' then 1 else 0) 


mostCommonBit :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
mostCommonBit (n, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) = (if div n 2 < b1 then 1 else 0,if div n 2 < b2 then 1 else 0,if div n 2 < b3 then 1 else 0,if div n 2 < b4 then 1 else 0,if div n 2 < b5 then 1 else 0,if div n 2 < b6 then 1 else 0,if div n 2 < b7 then 1 else 0,if div n 2 < b8 then 1 else 0,if div n 2 < b9 then 1 else 0,if div n 2 < b10 then 1 else 0,if div n 2 < b11 then 1 else 0,if div n 2 < b12 then 1 else 0)

leastCommonBit :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
leastCommonBit (n, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) = (if div n 2 >= b1 then 1 else 0,if div n 2 >= b2 then 1 else 0,if div n 2 >= b3 then 1 else 0,if div n 2 >= b4 then 1 else 0,if div n 2 >= b5 then 1 else 0,if div n 2 >= b6 then 1 else 0,if div n 2 >= b7 then 1 else 0,if div n 2 >= b8 then 1 else 0,if div n 2 >= b9 then 1 else 0,if div n 2 >= b10 then 1 else 0,if div n 2 >= b11 then 1 else 0,if div n 2 >= b12 then 1 else 0)

toupleToNumber :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Int
toupleToNumber (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) = b1 * 2^11 +b2 * 2^10 +b3 * 2^9 +b4 * 2^8 +b5 * 2^7 +b6 * 2^6 +b7 * 2^5 +b8 * 2^4 +b9 * 2^3 +b10 * 2^2 +b11 * 2^1 +b12 * 2^0 

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc05\\input.txt"    

    let inputList = words $ contents 
        bitCount = countBits inputList (0,0,0,0,0,0,0,0,0,0,0,0,0)
        gamaRate = mostCommonBit bitCount
        epsilonRate = leastCommonBit bitCount
        result = (toupleToNumber gamaRate) * (toupleToNumber epsilonRate)
        strRes = show gamaRate ++ "," ++ show epsilonRate ++ " -> " ++ show result 

    print strRes
    
