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
   strs = transpose $ explodeLines $ lines str
 in
  intersperse '\n' $ fmap concat strs


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
