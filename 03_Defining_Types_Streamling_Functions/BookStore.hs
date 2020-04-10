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
