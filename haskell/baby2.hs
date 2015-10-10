lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)
head' :: [a] -> a
head' [] = error "List is empty"
head' (x:_) = x
tail' :: [a] -> [a]
tail' [] = error "list is empty"
tail' (_:x) = x
length' :: (Num b) => [a] -> b
length' [] =0
length' (_:xs) = length' xs +1
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:y) = x + sum' y
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "te dun"
    | bmi <= normal = "normaal"
    | bmi <= fat = "te dik"
    | otherwise = "veel te dik"
    where bmi = weight / height ^2 
          skinny = 18.5
          normal = 25.0
          fat = 30
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w , h) <- xs]
    where bmi w h = w/h ^2
cylinder :: (RealFloat a ) => a -> a-> a
cylinder r h =
    let sideArea = 2* pi * r * h
        topArea  = pi * r^2 
    in sideArea +2 * topArea
fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n= fibonacci (n-1) + fibonacci (n-2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [a | a<- xs, a<= x] ++ [x] ++ quicksort [a | a<- xs, a> x]
mergeSort1 :: (Ord a) => [a] -> [a] -> [a]
mergeSort1 [] ys = ys
mergeSort1 xs [] = xs
mergeSort1 (x:xs) (y:ys)
    | x<= y = x:mergeSort1 xs (y:ys)
    | x>y = y:mergeSort1 (x:xs) ys
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [x] = [x]
mergesort (x:[xs]) = mergeSort1 [x] (mergeSort1 [xs])
