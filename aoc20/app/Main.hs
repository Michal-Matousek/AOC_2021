module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Array.MArray
import Debug.Trace


charValue :: Char -> Int
charValue ')' = 1
charValue ']' = 2
charValue '}' = 3
charValue '>' = 4
charValue '.' = 0

closingChar :: Char -> Char
closingChar c
    | c == '(' = ')'
    | c == '[' = ']'
    | c == '{' = '}'
    | c == '<' = '>'
    | c == '.' = '.'

isOpenieng,isClosing :: Char -> Bool
isOpenieng c = c == '(' || c == '[' || c == '{' || c == '<'
isClosing  c = c == ')' || c == ']' || c == '}' || c == '>'

    

corruptedChar :: String -> Char
corruptedChar s = crChr
    where (_, crChr) = corruptedCharRec (s,'.')

corruptedCharRec :: (String,Char) -> (String,Char)
corruptedCharRec ([] , cIn) = ([], '.')
corruptedCharRec ((c:cx),cIn) 
    | c == closingChar cIn = (cx,'.')    
    | c == '(' || c == '[' || c == '{' || c == '<' = if (crChr /= '.') then ([], crChr) else corruptedCharRec (cIn:restCX,'.')
    | otherwise = ([],c) 
        where (restCX, crChr) = corruptedCharRec (cx,c)

completeLine :: String -> String
completeLine s = if sRest == "" then missingStr else completeLine sRest
    where (_, sRest, missingStr) = completeLineRec ('.', s , "")

completeLineRec:: (Char, String, String) -> (Char, String, String)
completeLineRec (cIn, [] , mStr) = ('.', [], mStr ++ [closingChar cIn] )
completeLineRec (cIn, (c:cx), mStr) 
    | c == closingChar cIn = ('.', cx, mStr)    
    | c == '(' || c == '[' || c == '{' || c == '<' = (completeLineRec ('.', (cIn:restCX), restMsStr)  )
        where ( _, restCX, restMsStr) = (completeLineRec (c, cx, mStr))
        --trace ("compOpen " ++ show [c] ++ " " ++ show cx ++ " mstr:" ++ show mStr ) 

calculateScore :: String -> Int -> Int
calculateScore []  n = n
calculateScore (x:sx) n = calculateScore sx (5*n +  charValue x)

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc19\\input.test"    

    let lineIn = (lines $ contents)
        incompleteLines = [  l | l<-lineIn, '.' == corruptedChar l  ]
   
        -- ic = ["[(()[<>])]({[<{<<[]>>("] 
        completitionStrings = [ completeLine l | l<-incompleteLines ]
        scores = sort [calculateScore l 0 | l<-completitionStrings]
        pos = div (length scores) 2 
        result = scores !! pos
       
   
    print scores
    print result
    

    
    

    

    
