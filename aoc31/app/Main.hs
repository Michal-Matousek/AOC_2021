module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Array
import Data.Ix
import Data.Char
import Data.Array.MArray
import Debug.Trace

hexToBinary :: String -> String
hexToBinary [] = []
hexToBinary ('0':xs) = "0000" ++ (hexToBinary xs)
hexToBinary ('1':xs) = "0001" ++ (hexToBinary xs)
hexToBinary ('2':xs) = "0010" ++ (hexToBinary xs)
hexToBinary ('3':xs) = "0011" ++ (hexToBinary xs)
hexToBinary ('4':xs) = "0100" ++ (hexToBinary xs)
hexToBinary ('5':xs) = "0101" ++ (hexToBinary xs)
hexToBinary ('6':xs) = "0110" ++ (hexToBinary xs)
hexToBinary ('7':xs) = "0111" ++ (hexToBinary xs)
hexToBinary ('8':xs) = "1000" ++ (hexToBinary xs)
hexToBinary ('9':xs) = "1001" ++ (hexToBinary xs)
hexToBinary ('A':xs) = "1010" ++ (hexToBinary xs)
hexToBinary ('B':xs) = "1011" ++ (hexToBinary xs)
hexToBinary ('C':xs) = "1100" ++ (hexToBinary xs)
hexToBinary ('D':xs) = "1101" ++ (hexToBinary xs)
hexToBinary ('E':xs) = "1110" ++ (hexToBinary xs)
hexToBinary ('F':xs) = "1111" ++ (hexToBinary xs)

binToNum :: String -> Int -> Int
binToNum [] n =  n
binToNum ('0':xs) n =  binToNum xs (2*n)
binToNum ('1':xs) n =  binToNum xs (2*n+1)

decodeT4 :: String -> Int -> (Int, String)
decodeT4 [] n = (n,"")
decodeT4 (x:xs) n 
    |  x == '0'   =  (nextN, nextDigitStr)
    |  x == '1'   =  decodeT4 nextDigitStr (nextN)
    where digit = binToNum (take 4 xs) 0
          nextDigitStr = drop 4 xs
          nextN = 16*n + digit


-- version type value [subNodes]
data Packet = EmptyNode | Node Int Int Int [Packet] deriving (Show, Read, Eq)


decodePackets :: String -> (Packet, String)
decodePackets []  = (EmptyNode, [])
decodePackets msg = (Node versionNum typeNum value subNodes, restStr)
    where versionNum = binToNum (take 3 msg) 0
          typeMsg = drop 3 msg
          typeNum = binToNum (take 3 typeMsg) 0

          -- Node Type == 4 - literal number
          contentStr = drop 3 typeMsg
          (t4val, t4rest) = if (typeNum == 4) then decodeT4 contentStr 0 else (0,"")

          -- Node Type /= 4 - operator
          (lengthType:lenghtStr) = drop 3 typeMsg
          
          (lenghtBits, subPackBits) = if (lengthType == '0') then (take 15 lenghtStr, drop 15 lenghtStr) -- lenght in bits
                                                             else (take 11 lenghtStr, drop 11 lenghtStr) -- length in subPockets
          lenghtVal = binToNum lenghtBits 0
          (decodedSubPackets, opRest) = if (lengthType == '0') then decodePacketsLst (-1) (take lenghtVal subPackBits)
                                                               else (decodePacketsLst lenghtVal subPackBits)
          
          subPackRest = if (lengthType == '0') then drop lenghtVal subPackBits else ""
          
          subNodes = if (typeNum == 4) then [] else decodedSubPackets
          restStr  = if (typeNum == 4) then t4rest else (opRest ++ subPackRest)
          value    = if (typeNum == 4) then t4val else lenghtVal

decodePacketsLst :: Int -> String -> ([Packet], String)
decodePacketsLst (-1) [] = ( [] , [])
decodePacketsLst (-1) (x:xs) = ( decPacket:decRest, emptString )
    where (decPacket, restStr) = (decodePackets (x:xs))
          (decRest, emptString) = (decodePacketsLst (-1) restStr)
decodePacketsLst 0 msg = ( [] , msg)
decodePacketsLst i msg = ( decPacket:decRest, emptString )
    where (decPacket, restStr) = (decodePackets (msg))
          (decRest, emptString) = (decodePacketsLst (i-1) restStr)


sumVersions :: Packet -> Int
sumVersions EmptyNode = 0 :: Int
sumVersions (Node versionNum typeNum value subNodes) = versionNum + sum [ sumVersions sn  | sn <- subNodes]

evalPackets :: Packet -> Int
evalPackets EmptyNode = 0 :: Int
evalPackets (Node versionNum 4 value subNodes) = value
evalPackets (Node versionNum 0 value subNodes) = sum (map evalPackets subNodes)
evalPackets (Node versionNum 1 value subNodes) = product (map evalPackets subNodes)
evalPackets (Node versionNum 2 value subNodes) = minimum (map evalPackets subNodes)
evalPackets (Node versionNum 3 value subNodes) = maximum (map evalPackets subNodes)
evalPackets (Node versionNum 5 value subNodes) = if (evalPackets (head subNodes)) > (evalPackets (last subNodes)) then 1 else 0
evalPackets (Node versionNum 6 value subNodes) = if (evalPackets (head subNodes)) < (evalPackets (last subNodes)) then 1 else 0
evalPackets (Node versionNum 7 value subNodes) = if (evalPackets (head subNodes)) == (evalPackets (last subNodes)) then 1 else 0


main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc31\\input.txt"

    let -- INPUT                
        transmission = init $ contents
        binTrans = hexToBinary transmission

        (packets, _) = decodePackets binTrans

        verSum = sumVersions packets

    print "---------------------------------------------"            
    print packets
    print verSum
    

    


    

    
    

    

    
