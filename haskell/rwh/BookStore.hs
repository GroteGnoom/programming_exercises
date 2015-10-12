data BookInfo = Book Int String [String]
	deriving (Show)

data MagazineInfo = Magazine Int String [String]
	deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

type CustomerId = Int

type ReviewBody = String

data BookReview = BookReview BookInfo CustomerId String

data BetterReview = BetterReview BookInfo CustomerId ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String

type CardNumber = String

type Address = [String]


data BillingInfo = CreditCard CardNumber CardHolder Address
	| CashOnDelivery
	| Invoice CustomerId
	deriving (Show)


