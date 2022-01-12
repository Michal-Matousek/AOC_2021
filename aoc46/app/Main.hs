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

        -- solved by hand - there is only 1 solution
        -- D column must be freed - therea are only 2 option for that - 1 of them AB ... CB is immediately stuck as A is locked behind B
        -- so the only possible variant is BA ... BC
        -- C goes out so we can strt filing A column and free space from B (from C column)
        -- C column is free all Cs go there and finally B
   
        result = 41121
     

    print "---------------------------------------------"                            
    print result 
    
   
   
    




    

    


    

    
    

    

    
