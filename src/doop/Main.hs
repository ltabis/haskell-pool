-- | Main.hs
-- | Lucas Tabis
-- | File containing snd year haskell pool from Epitech, day 2.

module Main where

import System.Environment
import System.Exit
import Data.Maybe
import DoOp

-- | list of computation functions.
operations :: [(Int -> Int -> Int)]
operations = [doMod, doDiv, doMul, doSub, doSum]

-- | Main function.
main = do { args <- getArgs
          ; if (length args) /= 3
            then exitFormat 84
            else exitFormat (checkArgs (args!!0) (args!!2) (args!!1))
          }

-- | Format the exit status of the program.
exitFormat :: Int -> IO ()
exitFormat 0 = exitWith $ ExitSuccess
exitFormat n = exitWith $ ExitFailure n

-- | Checks wheter args are correct.
checkArgs :: String -> String -> String -> Int
checkArgs n m op | readInt n == Nothing              = 84
                 | readInt m == Nothing              = 84
                 | checkOperator op "+-*/%" == -1    = 84
                 | otherwise = fromMaybe 84 (maybeDo operation (readInt n) (readInt m))
                 where
                     operation = operations!!(checkOperator op "+-*/%")

-- | Check if the operator is correct.
checkOperator :: String -> [Char] -> Int
checkOperator op []     = -1
checkOperator op (x:xs) | (op!!0) /= x = checkOperator op xs
                        | otherwise = length xs

-- | Sum of two integers.
doSum :: Int -> Int -> Int
doSum n m = n + m

-- | Subtraction of two integers.
doSub :: Int -> Int -> Int
doSub n m = n - m

-- | Mutliplication of two integers.
doMul :: Int -> Int -> Int
doMul n m = n * m

-- | Division of two integers.
doDiv :: Int -> Int -> Int
doDiv n 0 = 84
doDiv n m = n `div` m

-- | Modulo of two integers.
doMod :: Int -> Int -> Int
doMod n m = n `mod` m