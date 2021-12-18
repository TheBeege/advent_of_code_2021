module Main where

import System.IO
import Control.Monad
import System.Environment
import System.Exit

import Lib

main = do
    args <- getArgs
    fileLines <- parse args
    let intList = inputLines fileLines
     in putStrLn $ show (solve intList)

parse ["-h"] = usage >> exit
parse []     = getContents
parse fs     = concat `fmap` mapM readFile fs

usage = putStrLn "Usage: main [-h] file"
exit  = exitSuccess
die   = exitWith (ExitFailure 1)

readInt :: String -> Int
readInt = read
inputLines :: String -> [Int]
inputLines inString = 
    let inLine = lines inString
     in map readInt inLine

solve :: [Int] -> Int
solve [] = error "no inputs!"
solve intList = do
    let zippedPairs = zip (init intList) (tail intList)
     in sum [1 | (x, y) <- zippedPairs, y > x]

