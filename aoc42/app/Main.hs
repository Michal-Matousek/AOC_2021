module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace

updateGameResultsFor :: Int -> Array (Int,Int,Int,Int) [Int] -> Array (Int,Int,Int,Int) [Int]
updateGameResultsFor i gme 
    | i == 0            = updGme
    | i > 0             = (updateGameResultsFor (i-1) updGme)
    where   
            updGmeP1 = gme // [ ((i,p,l1,l2), (winsFrom i p l1 l2 gme))  | p<-[0..i], l1<-[1..10] , l2<-[1..10] ]
            updGme = updGmeP1 //  [ ((p,i,l1,l2), (winsFrom p i l1 l2 updGmeP1)) | p<-[0..(i-1)], l1<-[1..10] , l2<-[1..10] ]

winsFrom :: Int -> Int -> Int -> Int -> Array (Int,Int,Int,Int) [Int] -> [Int]
winsFrom p1 p2 l1 l2 gme =  -- trace ("Point:" ++ (show  (p1,p2,l1,l2) ) ++ " -> "  ++ (show [p1winsSum, p2winsSum] )) 
                            [p1winsSum, p2winsSum]
    where posInc = [(1,3), (3,4), (6,5), (7,6), (6,7), (3,8), (1,9)]
          nPos = [ (count, (mod (l1 + pInc - 1) 10 ) + 1) | (count ,pInc) <- posInc ]
          nPosPoints = [ (count, pos, min (p1 + pos) 21 ) | (count, pos) <- nPos ]                    
         
          winsP2 = [ (count , if nPoints >= 21 then  [0,1] else (gme!(p2, nPoints, l2, nPos ))) | (count, nPos, nPoints) <- nPosPoints ]  
          wins = [ [count*p1wins, count*p2wins]  | (count, [ p2wins, p1wins]) <- winsP2 ] 
          
          p1winsSum = sum [ p1wins  | [p1wins, p2wins] <- wins]
          p2winsSum = sum [ p2wins  | [p1wins, p2wins] <- wins]

main :: IO ()
main = do 
    let gameZero = listArray ((0,0,1,1),(20,20,10,10)) [ [(-1)] | p1<-[0..20], p2<-[0..20], l1<-[1..10] , l2<-[1..10] ]
        gameInit = gameZero // [ ( (20,p,l1,l2) ,[27,0]) | p<-[0..20], l1<-[1..10] , l2<-[1..10] ]
        gameInit20 = gameInit // [ ((p,20,l1,l2), (winsFrom p 20 l1 l2 gameInit)) | p<-[0..19], l1<-[1..10] , l2<-[1..10] ]

        resultAll = updateGameResultsFor 19 gameInit20
        rb = resultAll!(0,0,6,1)
        result = maximum (resultAll!(0,0,6,1))

    print "---------------------------------------------"                       
    print ("Result -> " ++ (show result)) -- 175731756652760
   
    
    
   
    




    

    


    

    
    

    

    
