module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace


playGame :: [Int] -> [Int] -> Int -> Int -> [Int] -> Int -> Int
playGame [p1,p2] dice planLimit pointLimit [points1, points2] diceRolls 
    | nPoints1 >= pointLimit            = points2 * nDiceRolls
    | otherwise                         = playGame [p2, nPos1] nDice planLimit pointLimit [points2, nPoints1] nDiceRolls
    where roll = sum (take 3 dice)
          nDice = drop 3 dice
          nDiceRolls = diceRolls + 3
          nPos1 = (mod (p1 + roll - 1) planLimit ) + 1
          nPoints1 = points1 + nPos1



main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc41\\input.txt" 

    let -- INPUT                
        posStr = map last (lines $ contents)
        posLst = [ read [p] | p<-posStr ] :: [Int]

        dice = cycle [1..100]

        result = playGame posLst dice 10 1000 [0,0] 0        
        

    print "---------------------------------------------"                    
    print result
    
   
    




    

    


    

    
    

    

    
