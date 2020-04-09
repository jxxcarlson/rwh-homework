
getNumber :: String -> [(Double, String)]
getNumber str = reads str

getNumbers strs =
  fmap (\x -> fst $ head x) $ filter (\x -> x /= []) $ fmap getNumber strs
