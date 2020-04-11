data BookInfo = Book Int String [String]
                deriving (Show)


myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

 type CardHolder = String
 type CardNumber = String
 type Address = [String]

 data BillingInfo = CreditCard CardNumber CardHolder Address
                  | CashOnDelivery
                  | Invoice CustomerID
                    deriving (Show)


bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

O'Sullivan, Bryan. Real World Haskell: Code You Can Believe In (p. 53). O'Reilly Media. Kindle Edition. 
