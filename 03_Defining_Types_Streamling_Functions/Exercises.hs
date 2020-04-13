import Data.List (sortBy)

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

-- Problems 4, 5.

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


-- Problem 6.

compareLength :: [a] -> [a] -> Ordering
compareLength a b
  | la > lb      = GT
  | la == lb     = EQ
  | la < lb      = LT
  where
    la = length a
    lb = length b

-- sortByLength [[1,2,3], [1,2], [2], [3,4,5,6]]
-- [[2],[1,2],[1,2,3],[3,4,5,6]]
sortByLength :: [[a]] -> [[a]]
sortByLength list =
  sortBy compareLength list


-- Probelm 7.


intersperse :: Eq a => a -> [[a]] -> [a]
intersperse i (x:xs)
    | length xs == 1    = x ++ [i] ++ head xs
    | length xs > 0    = x ++ [i] ++ (intersperse i xs)
    | xs == [ ]         = x
intersperse i [] = []


-- Problem 8.  Height of Tree

height :: Tree a -> Int
height Empty = 1
height (Node _ s t) =
  1 + max (height s) (height t)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)


-- Problems 9, 10.


-- *Main> direction p_ q_ r_
-- --> TurnRight
data Direction = TurnLeft | StraightAhead | TurnRight
   deriving Show

data Point = Point {x :: Double, y :: Double}
                   deriving (Eq, Show)

data Displacement = Displacement {dx :: Double, dy :: Double}
                   deriving (Eq, Show)

makeDisplacement :: Point -> Point -> Displacement
makeDisplacement p q =
   Displacement {dx = (x q) - (x p), dy = (y q) - (y p)}

rotateLeft :: Displacement -> Displacement
rotateLeft delta =
  Displacement { dx = -(dy delta), dy = (dx delta)}

dotProduct :: Displacement -> Displacement -> Double
dotProduct a b =
  (dx a) * (dx b) + (dy a) * (dy b)

norm :: Displacement -> Double
norm ds =
  sqrt $ dotProduct ds ds

normalize :: Displacement -> Displacement
normalize ds =
  Displacement { dx = (dx ds)/nrm , dy = (dy ds)/nrm  }
  where
    nrm = norm ds

p_ = Point { x = 1, y = 2}

q_ = Point { x = 3, y = 3}

r_ = Point { x = 4, y = 3}

pq_ = makeDisplacement p_ q_
qr_ = makeDisplacement q_ r_

-- dotProduct pq (rotateLeft pq)
-- -> 0.0

-- *Main> direction p_ q_ r_
-- --> TurnRight
direction :: Point -> Point -> Point -> Direction
direction p q r
  | dotProd < 0      = TurnRight
  | dotProd == 1.0   = StraightAhead
  | dotProd > 0       = TurnLeft
  where
    pq = normalize $ makeDisplacement p q
    qr = normalize $ makeDisplacement q r
    normal = rotateLeft pq
    dotProd = dotProduct normal qr
