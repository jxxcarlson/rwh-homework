-- Problem 1

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail x = Just $ tail x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast x = Just $ last x

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit x = Just $ init x

-- Problem 2

break_ :: (a -> Bool) -> [a] -> ([a], [a])
break_ pred list =
  if pred (head list) then
      ([head list], tail list)
    else
      break pred list

-- *Main> splitWith (\c -> c == 'x') "xxxaxxbxx"
-- --> ["xxx","a","xx","b","xx"]
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred list =
  case (break_ (not . pred) list) of
    ([], []) -> []
    (xs, [])-> [xs]
    ([],ys) -> [ys]
    (xs, ys) -> xs : (splitWith (not . pred) ys)


-- Problem 4

unConsString :: String -> (String, String)
unConsString "" = ("", "")
unConsString (x:xs) = ([x], xs)

explode :: String -> [String]
explode str =
  fmap (\c -> [c]) str

-- > explodeLines $ lines "abc\ndef\ngh"
-- --> [["a","b","c"],["d","e","f"],["g","h"]]
explodeLines :: [String] -> [[String]]
explodeLines lines = fmap explode $ lines

stringHead :: [String] -> String
stringHead [] = ""
stringHead (x:xs) = x

stringTail :: [String] -> [String]
stringTail [] = []
stringTail str = tail str


headStrings :: [[String]] -> [String]
headStrings strs = fmap stringHead strs

tailStrings :: [[String]] -> [[String]]
tailStrings strs = fmap stringTail strs


isEmpty :: [String] -> Bool
isEmpty strs =
  and $ fmap (\s -> s == "") strs

transpose_ :: [[String]] -> [[String]]
transpose_ strs =
  let
    h = headStrings strs
    t = tailStrings strs
  in
  if isEmpty h then
    t
  else
   (h:(transpose t))

transpose :: [[String]] -> [[String]]
transpose strs =
     filter (\list -> list /= []) $ transpose_ strs

 -- > transposeString "abc\ndef\ngh"
 -- --> "adg\nbeh\ncf"
 -- > transposeString $ transposeString a
 -- --> "abc\ndef\ngh
transposeString :: String -> String
transposeString str =
  let
    strs = transpose $ explodeLines $ lines str
  in
   intersperse '\n' $ fmap concat strs



intersperse :: Eq a => a -> [[a]] -> [a]
intersperse i (x:xs)
    | length xs == 1    = x ++ [i] ++ head xs
    | length xs > 0    = x ++ [i] ++ (intersperse i xs)
    | xs == [ ]         = x
intersperse i [] = []

maxOfList :: [Int] -> Int
maxOfList [] = 0
maxOfList (x:xs) =
  max x (maxOfList xs)

lengthOfLongestLine :: [String] -> Int
lengthOfLongestLine strs = maxOfList $ fmap length strs

padding :: Int -> String -> String
padding n str = concat $ take n $ repeat str

padRight :: Int -> String -> String -> String
padRight k x str =
  let
    n = max 0 (k - (length str) )
    padding_ = padding n x
  in
    str ++ padding_



a = "abc\ndef\ngh"

aa = [["a","b","c"],["d","e","f"],["g","h"]]
bb = [["a","b","c"],["d","e","f"],["g","h"], [""]]
cc = [["a","b","c"],["d","e","f"],["g","h"], [""], []]
