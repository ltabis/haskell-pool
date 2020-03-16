-- | DoOp.hs
-- | Lucas Tabis
-- | File containing snd year haskell pool from Epitech, day 2.

module DoOp where

import Data.Char

-- | Check if an element is contained in a list. Constrained by the equality function.
myElem :: Eq a => a -> [a] -> Bool
myElem elem [] = False
myElem elem (x:xs) | elem == x = True
                   | otherwise = myElem elem xs

-- | Perform a safe integer division by returning a Nothing value if / by 0.
safeDiv :: Int -> Int -> Maybe Int
safeDiv f 0 = Nothing
safeDiv f s = Just (f `div` s)

-- | Get an element of a list by index, returns an error if the list is empty. Exept it uses Maybe to handle errors.
safeNth :: [a] -> Int -> Maybe a
safeNth [] n = Nothing
safeNth (x:xs) n | length (x:xs) < n  = Nothing
                 | n == 0 = Just x
                 | otherwise = safeNth xs (n - 1)

-- | Incrementing an Integer wrapped in a Maybe.
safeSucc :: Maybe Int -> Maybe Int
safeSucc n = case n of
                Just n  -> Just (n + 1)
                Nothing -> Nothing

-- | Incrementing an Integer wrapped in a Maybe, using the >>= operator.
safeSuccOp :: Maybe Int -> Maybe Int
safeSuccOp n = n >>= (\x -> Just (x + 1))

-- | Incrementing an Integer wrapped in a Maybe, using the fmap functor.
safeSuccF :: Maybe Int -> Maybe Int
safeSuccF n = fmap (\x -> x + 1) n

-- | return the second element of a tuple if the first matches the one passed as parameter, wrapped in a Just.
myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup key []     = Nothing 
myLookup key (x:xs) | key == fst x = Just (snd x)
                    | otherwise = myLookup key xs

-- | Apply a function on both arguments passed as parameter if both are Just wrapped values.
maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo f Nothing b = Nothing
maybeDo f a Nothing = Nothing
maybeDo f (Just a) (Just b) = Just (f a b)

-- | Reads a String and transforms it into an Int if it can, wrapped in a Maybe.
readInt :: [Char] -> Maybe Int
readInt []     = Just 0
readInt (x:xs) | isDigit x == True =  maybeDo (\a b -> a + b) convert (readInt xs)
               | otherwise = Nothing
               where
                   convert = (maybeDo (\a b -> a * 10 ^ (length b)) (Just (read [x]::Int)) (Just xs))

-- | Reads a line from cin and get it's length.
getLineLength :: IO Int
getLineLength = do x <- getLine
                   return $ length x

-- | Prints a string passed as parameter and returns its length wrapped in an IO
printAndGetLength :: String -> IO Int
printAndGetLength xs = do putStr (xs ++ "\n")
                          return $ length xs

-- | Prints a box.
printBox :: Int -> IO()
printBox n | n <= 0 = return ()
           | otherwise = putStr "OK"

-- | Concat lines from cin, given the number of lines passed as parameter.
concatLines :: Int -> IO String
concatLines n | n <= 0 = return ""
              | otherwise = do { line <- getLine
                               ; next <- concatLines (n - 1)
                               ; return (line ++ next) 
                               }

-- | Convert a line from cin into an int if it can.
getInt :: IO (Maybe Int)
getInt = do { line <- getLine
            ; return $ readInt line
            }