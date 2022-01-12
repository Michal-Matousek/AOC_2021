module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace


getPathsFrom :: String -> [[String]] -> [String] -> [[String]]
getPathsFrom currentNode links currentPath 
    | isThisNodeValid == False  = []
    | isThisNodeValid == True  && currentNode == "end" = [ currentPath ++ [currentNode]]
    | isThisNodeValid == True  && currentNode /= "end" = foundPaths
    where isCaveSmall = isLower (head currentNode)
          isThisNodeValid = if True == isCaveSmall && elem currentNode currentPath then False else True
          destNodes = [ if n1 == currentNode then n2 else n1  | [n1,n2] <- links, n1 == currentNode || n2 == currentNode ]
          destStr = show destNodes
          foundPaths = concat [  (getPathsFrom node links (currentPath ++ [currentNode])) | node<-destNodes ]
--trace (show currentNode ++ " P:" ++ show currentPath ++ " D:" ++ show destStr)
              
main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc23\\input.txt"    

    let links = [ splitOn "-" l  | l <- (lines $ contents) ] -- start-A        

        paths = getPathsFrom "start" links []
        result = length paths
        
      
    print links
    print paths
    print result
    


    

    
    

    

    
