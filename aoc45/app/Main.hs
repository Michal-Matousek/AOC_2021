module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc45\\input.txt" 

    let -- INPUT                
        inLines = take 2 (drop 2 (lines $ contents))
        amphListDirty = map (splitOn "#")  inLines
        amphList = [ x | x<-(concat amphListDirty), x /= "", x /= "  " ]
        moveList = permutations [1..16]

        -- solved by hand - wtf
        result = 15 + 20 * 10 + 1200 + 13000
     

    print "---------------------------------------------"                            
    print result
    
   
   
    




    

    


    

    
    

    

    
