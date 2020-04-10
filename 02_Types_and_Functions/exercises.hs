
dropRight :: Int -> [a] -> [a]
dropRight k xs = reverse $ drop k $ reverse xs

lastButOne :: [a] -> a
lastButOne = last . (dropRight 1)
