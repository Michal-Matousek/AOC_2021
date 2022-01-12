module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace


countMoves :: Array (Int, Int) Char -> Int -> Int
countMoves oceanMap iteration 
    | hasMovedLeft == False && hasMovedDown == False  = iteration + 1
    | otherwise                                       = countMoves downMap (iteration + 1)
    where (hasMovedLeft, leftMap) = moveLeft oceanMap
          (hasMovedDown, downMap) = moveDown leftMap
 

moveLeft :: Array (Int, Int) Char -> ( Bool ,Array (Int, Int) Char )
moveLeft oceanMap = (hasMoved, updatedMap) 
    where   ( _ , (numRows, numCols) ) = bounds oceanMap
            mapUpdateList =  [ [ ((r,c), '.' ) , ((r,(mod c numCols) + 1), '>' )  ]  | r<-[1..numRows], c<-[1..numCols], oceanMap!(r, (mod c numCols) + 1  ) == '.', oceanMap!(r, c  ) == '>' ] 
            hasMoved = (mapUpdateList /= [])
            updatedMap = oceanMap // (concat mapUpdateList)

moveDown :: Array (Int, Int) Char -> ( Bool ,Array (Int, Int) Char )
moveDown oceanMap = (hasMoved, updatedMap)
    where   ( _ , (numRows, numCols) ) = bounds oceanMap
            mapUpdateList =  [ [ ((r,c), '.' ) , (((mod r numRows) + 1, c), 'v' )  ]  | r<-[1..numRows], c<-[1..numCols], oceanMap!( (mod r numRows) + 1, c  ) == '.', oceanMap!(r, c  ) == 'v' ] 
            hasMoved = (mapUpdateList /= [])
            updatedMap = oceanMap // (concat mapUpdateList)


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc49\\input.txt" 

    let -- INPUT                
        oceanMapLines =  [ l | l <- lines$contents, l /= ""]

        mapSize = (length oceanMapLines, length (head oceanMapLines) )

        oceanMapInit = listArray ((1,1), mapSize) (concat oceanMapLines)

        numMovesToStop = countMoves oceanMapInit 0
        


    print "---------------------------------------------"                            
    
    print numMovesToStop
    
   
   
    




    

    


    

    
    

    

    
