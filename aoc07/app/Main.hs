module Main where

import System.IO
import Data.List.Split
import Debug.Trace

-- Params:
--      [In]:
--          max Bingo index (upper search index - we have board with bingo on this iterration)
--          list fo drawn nnumbers
--          list of bingo boards
--      [Out]: 
--          (minBingo iterration, corresponding min Bingo board - remaining numbers)
findMinBingoBoard :: Int -> [String] -> [String] -> (Int, [[String]])
findMinBingoBoard iMax numList (emtyLine:l1:l2:l3:l4:l5:[]) = if iCurrent < iMax 
                                                                then (iCurrent, reducedBoard)
                                                                else (iCurrent, [["no board found"]])
    where currentBoardInInts = map words (l1:l2:l3:l4:l5:[])
          (iCurrent, reducedBoard) = processBoard iMax numList currentBoardInInts          

findMinBingoBoard iMax numList (emtyLine:l1:l2:l3:l4:l5:remainingBoards) = (min iCurrent iBest, resultBoard) 
    where currentBoardInInts = map words (l1:l2:l3:l4:l5:[])
          (iCurrent, reducedBoard) = processBoard iMax numList currentBoardInInts
          (iBest, bestBoard) = findMinBingoBoard (min iMax iCurrent) numList remainingBoards
          resultBoard = if iCurrent < iBest then reducedBoard else bestBoard

processBoard :: Int -> [String] -> [[String]] -> (Int, [[String]])
processBoard iMax numList boardLines = (i, take 5 board)
    where allLines = boardLines ++ transpose boardLines
          (i,board) = reduceTillEmpty 0 iMax numList allLines

reduceTillEmpty :: Int -> Int -> [String] -> [[String]] -> (Int, [[String]])
reduceTillEmpty i iMax numList boardLines
    | i >= iMax = (i+1, [["reduction limit reached - not BINGO board"]] )
    | i < iMax  = if isBingo == True then (i,reducedBoard) else reduceTillEmpty (i+1) iMax numList reducedBoard
    where reducedBoard = [ [ x | x<-lx, x /= numList!!i ] | lx<-boardLines ]
          isBingo = hasEmptyList reducedBoard

hasEmptyList :: [[String]] -> Bool
hasEmptyList (x:[]) = (x == [])
hasEmptyList (x:xs) = (x == []) || hasEmptyList xs

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

main :: IO ()
main = do 
    contents <- readFile  "D:\\Source\\AoC_2021\\aoc07\\input.txt"    

    let inputList = lines $ contents         
        numberList = splitOn "," (head (take 1 inputList))        
        boardsList = tail inputList

        (minNumber, minBoardStr) = findMinBingoBoard (length numberList) numberList boardsList
        minBoard = [ map read line | line<-minBoardStr] :: [[Int]]
        minBoardSum = sum (map sum minBoard)
        lastNum = read (numberList!!minNumber) :: Int

        result = minBoardSum * lastNum
        strRes = show minBoardSum ++ "," ++ show minNumber ++ " -> " ++ show result         

    print minBoard
    print strRes
