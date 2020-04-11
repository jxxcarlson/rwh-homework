-- Problem 1,2.

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

test :: [a] -> Bool
test list =
  length list == myLength list


-- Problem 3.

mean :: [Double] -> Double
mean list =
  sum list / n
  where
    n = fromIntegral $ length list

-- Problem 4.

dropAtEnd :: Int -> [a] -> [a]
dropAtEnd k list =
  reverse $ drop k $ reverse list

evenPalindrome :: [a] -> [a]
evenPalindrome list =
  list ++ reverse list

oddPalindrome :: [a] -> [a]
oddPalindrome list =
  halfList ++ [middle] ++ reverse halfList
  where
    middle = last list
    halfList = dropAtEnd 1 list

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list =
  isEvenPalindrome list || isOddPalindrome list

isEvenPalindrome :: Eq a => [a] -> Bool
isEvenPalindrome list =
  list == (evenPalindrome halfOfList)
  where
    n = length list
    halfOfList = take (div n 2) list

isOddPalindrome :: Eq a => [a] -> Bool
isOddPalindrome list =
  list == (oddPalindrome halfOfList)
  where
    n = length list
    halfOfList = take ((div n 2) + 1) list
