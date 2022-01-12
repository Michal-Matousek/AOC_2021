module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace

sumSNumbers :: String -> String -> String
sumSNumbers a b = reduceSnum ("[" ++ a ++ "," ++ b ++ "]")

sumSNumList :: String -> [String] -> String
sumSNumList a [] = a
sumSNumList a (x:xs) = sumSNumList (sumSNumbers a x) xs

reduceSnum :: String -> String
reduceSnum a
    | a /= exA              = reduceSnum exA
    | a == exA && splA /= a = reduceSnum splA
    | a == exA && splA == a = a
    where exA = explodeSNum a
          splA = splitSNum  a

explodeSNum :: String -> String
explodeSNum a = if afterPair == "" then frontPair else (addToLeftNum frontPair pairLnum) ++ "0" ++ (addToRightNum afterPair pairRnum)
    where (frontPair, pairLnum, pairRnum, afterPair) = partsForExplode a       

splitSNum :: String -> String
splitSNum a = if afterStr == "" then frontStr else frontStr ++ "[" ++ (show lNum) ++ "," ++ (show rNum) ++ "]" ++ afterStr
    where (frontStr, splitNum, afterStr) = partsForSplit a
          lNum = div (read splitNum) 2 :: Int
          rNum = (read splitNum) - lNum :: Int

partsForExplode :: String -> (String, String, String, String)
partsForExplode a = searchExplodePair 0 ("", "", "", a) 

searchExplodePair :: Int -> (String, String, String, String) -> (String, String, String, String)
searchExplodePair i (a,b,c,[]) = (a,b,c,[])
searchExplodePair 4 (a,b,c, ('[':xs) ) = (a, head pairParts, last pairParts, stringRest)
    where parts = splitOn "]" xs
          pairParts = splitOn "," (head parts)
          stringRest = drop ( 1 + length (head parts)) xs
searchExplodePair i (a,b,c, ('[':xs) ) = searchExplodePair (i+1) ((a ++ ['[']),b,c, xs )
searchExplodePair i (a,b,c, (']':xs) ) = searchExplodePair (i-1) ((a ++ [']']),b,c, xs )
searchExplodePair i (a,b,c, (x:xs) ) = searchExplodePair i ((a ++ [x]),b,c, xs )

partsForSplit :: String -> (String, String, String)
partsForSplit a 
    | rightPart == ""                     = (leftPart, num, rightPart)
    | rightPart /= "" && numVal >= 10 = (leftPart, num, rightPart)
    | rightPart /= "" && numVal < 10  = (leftPart ++ num ++ recLeft, recNum, recRight)    
    where (leftPart, num, rightPart) = searchNum ("", "", a )
          (recLeft, recNum, recRight) = partsForSplit rightPart
          numVal = read num :: Int
          
searchNum :: (String, String, String) -> (String, String, String)
searchNum (a,b,[]) = (a,b,[])
searchNum (a,b, (',':xs) ) = searchNum (a ++ [','] , b , xs)
searchNum (a,b, ('[':xs) ) = searchNum (a ++ ['['] , b , xs)
searchNum (a,b, (']':xs) ) = searchNum (a ++ [']'] , b , xs)
searchNum (a,b, (x:xs) ) = (a , num , rest)
    where (num, rest) = if (head xs) >= '0' &&  (head xs) <= '9' then (x:(head xs):[], tail xs)
                                                                 else ([x], xs)


addToLeftNum :: String -> String -> String
addToLeftNum leftNumStr numStr = if rightPartRev == "" then leftNumStr
                                                       else leftPart ++ numSum ++ rightPart
    where num = read numStr :: Int
          revStr = reverse leftNumStr
          (leftPartRev, foundNumRev, rightPartRev) = searchNum ("","",revStr)
          leftPart = reverse rightPartRev
          rightPart = reverse leftPartRev
          foundNum = reverse foundNumRev
          foundNumVal = read foundNum :: Int
          numSum = show (num + foundNumVal)

addToRightNum :: String -> String -> String
addToRightNum rightNumStr numStr =  if rightPart == "" then rightNumStr
                                                       else leftPart ++ numSum ++ rightPart
    where num = read numStr :: Int
          (leftPart, foundNum, rightPart) = searchNum ("","",rightNumStr)
          foundNumVal = read foundNum :: Int
          numSum = show (num + foundNumVal)          

-- -> (leftPart, rightPart)
splitNumParts :: String -> (String, String)
splitNumParts [] = ("","") 
splitNumParts ('[':xs) = (leftPart,rightPart)
    where (_, leftPart, rightPart) = splitByComa (0, "", (init xs))

splitByComa :: (Int, String, String) -> (Int, String, String)
splitByComa ( i, frontStr,  []       )  = (0, frontStr, [])
splitByComa ( 0, frontStr,  (',':xs) )  = (0, frontStr,  xs)
splitByComa ( i, frontStr,  ('[':xs) )  = splitByComa( (i+1) ,frontStr++['['],  xs)
splitByComa ( i, frontStr,  (']':xs) )  = splitByComa( (i-1) ,frontStr++[']'],  xs)
splitByComa ( i, frontStr,  (x:xs) )    = splitByComa( i, frontStr++[x],  xs)

sNumMagnitude :: String -> Int
sNumMagnitude [] = 0
sNumMagnitude s = 3*leftVal + 2*rightVal
    where (leftPart,rightPart) = splitNumParts s
          leftVal = if (head leftPart) == '[' then sNumMagnitude leftPart else (read leftPart) :: Int
          rightVal = if (head rightPart) == '[' then sNumMagnitude rightPart else (read rightPart) :: Int

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc35\\input.txt" 

    let -- INPUT                
        linesIn = (lines $ contents)     -- [[[[0,6],[8,7]],[2,3]],[3,[[6,5],[0,0]]]]
        
        mags = maximum [ sNumMagnitude (sumSNumbers i j) | i<-linesIn, j<-linesIn, i /= j ]
        

        t = sNumMagnitude  "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"

    print "---------------------------------------------"            
    print mags
    
    




    

    


    

    
    

    

    
