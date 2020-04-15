import Data.Char (digitToInt)

asInt xs = loop 0 xs

loop acc [] = acc
loop acc (x:xs) =
  let acc' = (acc * 10) + digitToInt x
  in loop acc' xs

asInt' xs = foldl reducer 0 xs
   where reducer acc digit = 10 * acc  + digitToInt digit
