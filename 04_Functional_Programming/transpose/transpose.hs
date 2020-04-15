import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = transposeString



-- > transposeString "abc\ndef\ngh"
-- --> "adg\nbeh\ncf"
-- > transposeString $ transposeString a
-- --> "abc\ndef\ngh
transposeString :: String -> String
transposeString str =
 let
   strs = transpose $ explodeLines $ padLines $ lines str
 in
  (intersperse '\n' $ fmap concat strs) ++ "\n"


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

padLines :: [String] -> [String]
padLines strs =
  let
    n = lengthOfLongestLine strs
  in
  fmap (padRight n "*") strs
