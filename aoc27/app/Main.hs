module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace


polymerize :: Int -> [[String]] -> String -> String
polymerize 0 rules poly = poly
polymerize i rules poly = polymerize (i-1) rules nextPoly
    where nextPoly = polyStep rules poly

polyStep :: [[String]] -> String -> String
polyStep rules (x:[]) = x:[]
polyStep rules (a:b:px) = a:insertion:(polyStep rules (b:px))
    where insertion = head (head [ ins | [match,ins] <- rules, a:(b:[]) == match ])


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc27\\input.txt"

    let template =  head (lines $ contents)
        contentRest = tail (tail (lines $ contents))
        insertionRules = [ splitOn " -> " l  | l <- contentRest ] 


        polymer = polymerize 10 insertionRules template 
        grpSizes = map length (group (sort polymer))
        result =  (maximum grpSizes) - (minimum grpSizes)

    print "---------------------------------------------"    
    print polymer    
    print result

    


    

    
    

    

    
