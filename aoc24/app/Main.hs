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
          smallRepeat = if True == isCaveSmall && elem currentNode currentPath then True else False
          isThisNodeValid = if smallRepeat == False || (0 == smallRepeated currentPath && currentNode/="start") then True else False

          destNodes = [ if n1 == currentNode then n2 else n1  | [n1,n2] <- links, n1 == currentNode || n2 == currentNode ]
          destStr = show destNodes
          foundPaths = concat [  (getPathsFrom node links (currentPath ++ [currentNode])) | node<-destNodes ]
              

smallRepeated ::  [String] -> Int
smallRepeated [] = 0
smallRepeated (node:nx) 
    | isCaveSmall == False = smallRepeated nx
    | isCaveSmall == True =  nodeRepeated + smallRepeated nx
    where isCaveSmall = isLower (head node)
          nodeRepeated = if (elem node nx) then 1 else 0

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc23\\input.txt"    

    let links = [ splitOn "-" l  | l <- (lines $ contents) ] -- start-A        

        paths = getPathsFrom "start" links []
        result = length paths
      
    print result

    


    

    
    

    

    
