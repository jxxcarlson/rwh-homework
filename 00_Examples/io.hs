main :: IO ()
main = do
  putStrLn "Enter a number greater than 3: "
  x <- readLn
  print (x > 3)
