-- | My.hs
-- | Lucas Tabis
-- | File containing snd year haskell pool from Epitech.

-- increment an int by one.
mySucc :: Int -> Int
mySucc n = n + 1

-- Check wether an int is negative or not.
myIsNeg :: Int -> Bool
myIsNeg n | n < 0  = True
          | n >= 0 = False

-- Get the absolute value of an int.
myAbs :: Int -> Int
myAbs n | myIsNeg n == True  = n * (-1)
        | myIsNeg n == False = n

-- Get the min value of two ints.
myMin :: Int -> Int -> Int
myMin a b | a < b     = a
          | otherwise = b

-- Get the max value of two ints.
myMax :: Int -> Int -> Int
myMax a b | a > b     = a
          | otherwise = b

-- Create a tuple from two parameters.
myTuple :: a -> b -> (a , b)
myTuple a b = (a, b)

-- Create a truple from three parameters.
myTruple :: a -> b -> c -> (a , b, c)
myTruple a b c = (a, b, c)

-- Get the first element of a tuple.
myFst :: (a , b) -> a
myFst (a, b) = a

-- Get the second element of a tuple.
mySnd :: (a , b) -> b
mySnd (a, b) = b

-- Swap to elements from a tuple.
mySwap :: (a , b) -> (b , a )
mySwap (a, b) = (b, a)

-- Get the first element of a list, returns an error if the list is empty.
myHead :: [a] -> a
myHead xs = case xs of [] -> error "Error: empty string."
                       (x:_) -> x

-- Get a list without the first element, returns an error if the list is empty.
myTail :: [a] -> [a]
myTail xs = case xs of [] -> error "Error: empty string."
                       (_:xs) -> xs

-- Get the length of a list.
myLength :: [a] -> Int
myLength xs = case xs of [] -> 0
                         (_:xs) -> myLength xs + 1

-- Get an element of a list by index, returns an error if the list is empty.
myNth :: [a] -> Int -> a
myNth (x:xs) n | myLength (x:xs) == 0 = error "Error: empty string."
               | n == 0 = x
               | otherwise = myNth xs (n - 1)

-- Get all elements to 'n', dropping the rest of the list.
myTake :: Int -> [a] -> [a]
myTake n (x:xs) | myLength (x:xs) < n = (x:xs)
                | n == 0 = []
                | otherwise = x:(myTake (n - 1) xs)

-- Get all elements after the 'n' index, dropping the first elements.
myDrop :: Int -> [a] -> [a]
myDrop n (x:xs) | myLength (x:xs) < n = []
                | n == 0 = (x:xs)
                | otherwise = myDrop (n - 1) xs

-- Append two lists together
myAppend :: [a] -> [a] -> [a]
myAppend [] l2 = l2
myAppend l1 [] = l1
myAppend (x:l1) l2 | myLength l1 == 0 = (x:l2)
                   | otherwise = x:(myAppend l1 l2)

-- Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) [x]

-- Get the first element of a list, returns an error if the list is empty.
myInit :: [a] -> [a]
myInit [] = error "Error: empty string."
myInit (x:xs) | myLength (x:xs) == 1 = []
              | otherwise = x:(myInit xs)

-- Get the last element of a list, returns an error if the list is empty.
myLast :: [a] -> a
myLast [] = error "Error: empty string."
myLast (x:xs) | myLength (x:xs) == 1 = x
              | otherwise = myLast xs

-- transform two lists into a list of tuples.
myZip :: [a] -> [b] -> [(a, b)]
myZip [] x = []
myZip x [] = []
myZip (x1:xs1) (x2:xs2) = myAppend [(x1, x2)] (myZip xs1 xs2)

-- Unzip a list of tuples into a tuple of lists.
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip (x:xs) = myTuple (myAppend [elem1] (myFst next)) (myAppend [elem2] (mySnd next)) 
                 where
                    elem1 = myFst x
                    elem2 = mySnd x
                    next = myUnzip xs

-- Applies a function on every element of the array passed as parameter.
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = myAppend [f x] (myMap f xs)

-- Applies a function that returns true or false on all elements of an array. Return an array of all elements that thr function returned true for.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) | f x == True = myAppend [x] (myFilter f xs)
                  | otherwise = myFilter f xs

-- Apply the function passed as parameter on every element of the array and feeds the result to the function.
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f start [] = start
myFoldl f start (x:xs) = myFoldl f result xs
                         where
                             result = f start x

-- Same as myFoldl but from the right.
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f start [] = start
myFoldr f start xs = myFoldr f result (myInit xs)
                     where
                         result = f (myLast xs) start

-- Put elements that returned false when passed in a function into a tuple of lists. If the expression was true, it puts it in the first list, otherwise it puts it in the second.
mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan f [] = ([], [])
mySpan f (x:xs) | f x == True  = myTuple (myAppend [x] (myFst next)) (mySnd next)
                | otherwise    = myTuple (myFst next) (myAppend [x] (mySnd next))
                where
                    next = mySpan f xs

-- Uses a function passed as parameter to order a list.
-- myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
-- myQuickSort f [] = []
-- myQuickSort f (x:xs) = myAppend (myQuickSort (myFst next)) myAppend ([x] (myQuickSort (mySnd next)))
--                        where
--                            next = mySpan (\y -> y) (f x (myHead xs)) xs
