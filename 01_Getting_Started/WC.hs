-- file: ch01/WC.hs

wordsInList :: [String] -> [String]
wordsInList list = concat $ fmap words list

charsInList :: [String] -> String
charsInList list = concat $ wordsInList list

stats :: [String] -> [Int]
stats list = [length list, length $ wordsInList list, length $ charsInList list]

main = interact wordCount
      where wordCount input = show (stats $ lines input) ++ "\n"
