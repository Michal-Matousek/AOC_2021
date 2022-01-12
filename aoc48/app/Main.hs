module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace

initReg = array ('w','z') [(i,0) | i <- ['w'..'z']]

numToDigits :: Int -> [Int]
numToDigits 0 = []
numToDigits numIn = numToDigits (div numIn 10) ++ [mod numIn 10]

findZ0from :: [String] -> Int -> Int
findZ0from aluProg inNum 
        | (finalReg!'z') == 0      = inNum
        | (finalReg!'z') /= 0      = -- trace (show inNum ++ " -> " ++ (show (finalReg!'z'))) 
                                        (findZ0from aluProg (inNum-1))
        where   numDigits = numToDigits inNum
                finalReg = 
                                (runAlu aluProg numDigits initReg)


runAlu :: [String] -> [Int] -> Array Char Int -> Array Char Int
runAlu []  numDigits regs = regs
runAlu (inst:ix) numDigits regs 
        | insStr == "inp"       = runAlu ix restDigits (regs // [ (head (head paramStr), firstDigit) ])
        | insStr == "add"       = runAlu ix numDigits  (regs // [ (head a, bVal + aVal) ])
        | insStr == "mul"       = runAlu ix numDigits  (regs // [ (head a, bVal * aVal) ])
        | insStr == "div"       = runAlu ix numDigits  (regs // [ (head a, div aVal bVal) ])
        | insStr == "mod"       = runAlu ix numDigits  (regs // [ (head a, mod aVal bVal) ])
        | insStr == "eql"       = runAlu ix numDigits  (regs // [ (head a, (if aVal == bVal then 1 else 0)) ])

        where (insStr:paramStr) = splitOn " " inst
              [a,b]  = paramStr
              firstDigit:restDigits = numDigits              
              bVal = if (head b) >= 'w' && (head b) <= 'z' then (regs!(head b)) else read b
              aVal = (regs!(head a))



main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc47\\input.txt" 

    let -- INPUT                
        aluProg =  (lines $ contents)
        
        -- highestNum = findZ0from aluProg 99999999999999
        -- highestNum = runAlu aluProg [9,9,9,1,1,9,9,3,9,4,9,6,8,4] initReg
        highestNum = runAlu aluProg [6,2,9,1,1,9,4,1,7,1,6,1,1,1] initReg
        

        


    print "---------------------------------------------"                            
    
    print highestNum
    
   
   
    




    

    


    

    
    

    

    
