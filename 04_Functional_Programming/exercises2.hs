import Data.Char (digitToInt)

asInt xs = loop 0 xs

loop acc [] = acc
loop acc (x:xs) =
  let acc' = (acc * 10) + digitToInt x
  in loop acc' xs

asInt' xs = foldl reducer 0 xs
   where reducer acc digit = 10 * acc  + digitToInt digit



asInt2 :: String -> Int
asInt2 [] = error "Can't handle empty string"
asInt2 "-" = error "Can't handle '-' unless digits following"
asInt2 (x:xs)
   | x == '-'       = -(asInt' xs)
   | otherwise      = asInt' (x:xs)
