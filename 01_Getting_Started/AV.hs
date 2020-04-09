main :: IO ()
main = do
  putStrLn "Enter numbers to average: "
  str <- getLine
  print  (average $ getNumbers str)


getNumber :: String -> [(Double, String)]
getNumber str = reads str

toNumbers :: [String] -> [Double]
toNumbers strs =
  fmap (\x -> fst $ head x) $ filter (\x -> x /= []) $ fmap getNumber strs

getNumbers :: String -> [Double]
getNumbers str = toNumbers $ words str

average :: [Double] -> Double
average xs =
  sum xs / (fromIntegral $ length xs)


-- average :: String -> Float
-- average str =
--   let
--     xs = fmap read $ words str
