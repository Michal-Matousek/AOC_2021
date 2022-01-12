module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace

charToByte :: Char -> Int
charToByte c = if c == '#' then 1 else 0

convLine :: Array Int Int -> [[Int]] -> [[Int]]
convLine algo [] = []
convLine algo (a:[]) = []
convLine algo (a:b:[]) = []
convLine algo (a:b:c:lx) = lineAfterConv : (convLine algo (b:c:lx))
    where lineAfterConv = convRec algo a b c

convRec :: Array Int Int -> [Int] -> [Int] -> [Int] -> [Int]
convRec algo [] _ _ = []
convRec algo (b0:[]) _ _ = []
convRec algo (b0:b1:[]) _ _ = []
convRec algo (b0:b1:b2:bAs) (b3:b4:b5:bBs) (b6:b7:b8:bCs) = algo!index : (convRec algo (b1:b2:bAs) (b4:b5:bBs) (b7:b8:bCs))
    where index = (((((((b0*2 + b1)*2 + b2)*2 + b3)*2 + b4)*2+b5)*2+b6)*2+b7)*2+b8


doConvolution :: Int -> Array Int Int -> [[Int]] -> Int -> [[Int]]
doConvolution 0 _ mapLines boundFill = mapLines
doConvolution i algo mapLines boundFill = doConvolution (i-1) algo convMapLines newBoundFill
    where   (szX,szY) = ( length mapLines, length (head mapLines) )
            boundLine = take (szY+6) (repeat boundFill) :: [Int]
            boundMapLines = [boundLine] ++ [boundLine] ++ [boundLine] ++ [ [boundFill,boundFill,boundFill] ++ l ++ [boundFill,boundFill,boundFill] | l<-mapLines] ++ [boundLine] ++ [boundLine] ++ [boundLine] 
            convMapLines = convLine algo boundMapLines
            newBoundFill = if boundFill == 0 then (algo!0) else (algo!(2^9 - 1))


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc39\\input.txt" 

    let -- INPUT                
        algo = head (lines $ contents)   -- ..##..  
        oceanMap = drop 2 (lines $ contents)
        lineLength = length (head oceanMap)
        mapLines = [ map charToByte l | l<-oceanMap, l /= "" ]

        algoArr = listArray (0,2^9 - 1) (map charToByte algo)

        convMap1 = doConvolution 2 algoArr mapLines 0        
        convMap = convMap1

        t1 = [ [if c == 0 then '.' else '#'| c<-l] | l<-convMap1]


        str1 = concat (intersperse "\n"  t1)
        t= convRec algoArr [0,0,0] [0,0,0] [0,0,0]
        
                
        result = sum (concat convMap)

        

    print "---------------------------------------------"                    
    print ( length mapLines, length (head mapLines) )
    
    print ("Result:" ++ (show result))
    putStrLn str1
    print "---------------"
    print ("0:" ++ show (algoArr!0))
    print ("1:" ++ show (algoArr!511))
    print t

    -- 5551 to low
    -- 6171 to high
    -- 5622
    




    

    


    

    
    

    

    
