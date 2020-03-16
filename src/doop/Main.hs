-- | Main.hs
-- | Lucas Tabis
-- | File containing snd year haskell pool from Epitech, day 2.

module Main where

import System.Environment
import Data.Maybe
import DoOp

operations :: [(Int -> Int -> Int)]
operations = [doMod, doDiv, doMul, doSub, doSum]

main = do { args <- getArgs
          ; if (length args) /= 3
            then return 84
            else do print $ checkArgs (args!!0) (args!!2) (args!!1)
                    return $ checkArgs (args!!0) (args!!2) (args!!1)
          }

checkArgs :: String -> String -> String -> Int
checkArgs n m op | readInt n == Nothing              = 84
                 | readInt m == Nothing              = 84
                 | checkOperator op "+-*/%" == -1    = 84
                 | otherwise = fromMaybe 84 (maybeDo operation (readInt n) (readInt m))
                 where
                     operation = operations!!(checkOperator op "+-*/%")

checkOperator :: String -> [Char] -> Int
checkOperator op []     = -1
checkOperator op (x:xs) | (op!!0) /= x = checkOperator op xs
                        | otherwise = length xs

doSum :: Int -> Int -> Int
doSum n m = n + m

doSub :: Int -> Int -> Int
doSub n m = n - m

doMul :: Int -> Int -> Int
doMul n m = n * m

doDiv :: Int -> Int -> Int
doDiv n 0 = 84
doDiv n m = n `div` m

doMod :: Int -> Int -> Int
doMod n m = n `mod` m