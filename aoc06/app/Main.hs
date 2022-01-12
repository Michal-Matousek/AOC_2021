module Main where

import System.IO

-- For single string in list just convert to number
-- For longer list:
--        - split input list by "i" bit to lists with corresponding digit '1' and '0'
--        - identify longer a and shorter list
--        - filter by subsequent bit
splitListByBitRec :: [String] -> Int -> (Int, Int)
splitListByBitRec (x:[]) _ = (num,num)
    where num = bitString2Num x 0

splitListByBitRec inStrList i = (longerListRec, shorterListRec)
    where list1 = [ x | x<-inStrList, x!!i == '1' ]
          list0 = [ x | x<-inStrList, x!!i == '0' ]
          (longerList, shorterList) = if length list1 >= length list0 then (list1, list0) else (list0,list1)
          (longerListRec,_) = splitListByBitRec longerList (i+1)
          (_,shorterListRec) = splitListByBitRec shorterList (i+1)

-- Just convert binary number in String to Number
-- remaining string, cummulated value, result
bitString2Num :: String -> Int -> Int
bitString2Num [] n = n
bitString2Num ('0':sx) n = bitString2Num sx (2*n)
bitString2Num ('1':sx) n = bitString2Num sx (2*n+1)

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc05\\input.txt"    

    let inputList = words $ contents         

        (oxygenRating,co2Rating)  = splitListByBitRec inputList 0         

        result = oxygenRating * co2Rating
        strRes = show oxygenRating ++ "," ++ show co2Rating ++ " -> " ++ show result 

    print strRes
    
