module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace


getRulesHist :: Int -> [[String]] -> Array (Char,Char) (Array Char Int)
getRulesHist 0 rules = array (('A','A'),('Z','Z'))  [ ((a,b), singleRuleHist (a,b) ) | [a:b:_,_] <- rules ] 
getRulesHist iter rules = array (('A','A'),('Z','Z')) ( [ ((a,b), (sumHist [prevHist!(a,c), prevHist!(c,b), delOneCharHist c ]) ) | [a:b:_,c:_] <- rules ] )
    where prevHist = getRulesHist (iter-1) rules                    
          

sumHist ::  [Array Char Int] -> Array Char Int
sumHist    []  = accumArray (+) 0 ('A','Z')  [] 
sumHist (x:[]) = x
sumHist (x:xs) = array ('A','Z') ( [ (i, x!i + prevHist!i)  | i<-['A'..'Z'] ] )
    where prevHist = sumHist xs

singleRuleHist :: (Char,Char) -> Array Char Int
singleRuleHist (a,b) = accumArray (+) 0 ('A','Z')  [ (a,1), (b,1) ] 

delOneCharHist :: (Char) -> Array Char Int
delOneCharHist a = accumArray (+) 0 ('A','Z')  [ (a,-1) ] 

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc27\\input.txt"

    let -- INPUT        
        template =  head (lines $ contents)
        contentRest = tail (tail (lines $ contents))
        insertionRules = [ splitOn " -> " l  | l <- contentRest ] 

        -- PROCESSING
        rulesHist = getRulesHist 40 insertionRules
        templateDoubles = zip (init template) (tail template)
        templateHistWithDupl = sumHist [ rulesHist!(a,b)  | (a,b) <- templateDoubles  ]

        templateMiddle = tail (init template)
        templateDupl = accumArray (+) 0 ('A','Z')  [ (c, -1) | c <- templateMiddle ]
        templateHist = sumHist [templateHistWithDupl,templateDupl]
        
        -- CLCULATE RESULT
        charHistNo0 = [ templateHist!i | i <- ['A'..'Z'] ,  templateHist!i > 0 ]
        charHistMax = maximum charHistNo0
        charHistMin = minimum charHistNo0

        result =  charHistMax - charHistMin

    print "---------------------------------------------"
    print result
    

    


    

    
    

    

    
