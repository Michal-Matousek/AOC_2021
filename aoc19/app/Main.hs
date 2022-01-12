module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Array.MArray
import Debug.Trace


charValue :: Char -> Int
charValue ')' = 3
charValue ']' = 57
charValue '}' = 1197
charValue '>' = 25137
charValue '.' = 0

closingChar :: Char -> Char
closingChar c
    | c == '(' = ')'
    | c == '[' = ']'
    | c == '{' = '}'
    | c == '<' = '>'
    | c == '.' = '.'
    

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

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc19\\input.txt"    

    let lineIn = (lines $ contents)
        wrongChars = [ corruptedChar l | l<-lineIn, '.' /= corruptedChar l  ]        
        charsValue = sum [charValue c| c<-wrongChars]    
    

    print wrongChars
    print charsValue
    

    

    
