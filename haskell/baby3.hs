splitlist x = splitAt ((length x) `div` 2) x
mergeSort1 :: (Ord a) => [a] -> [a] -> [a]
mergeSort1 [] ys = ys
mergeSort1 xs [] = xs
mergeSort1 (x:xs) (y:ys)
    | x<= y = x:mergeSort1 xs (y:ys)
    | x>y = y:mergeSort1 (x:xs) ys
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSort1 (mergeSort(fst (splitlist xs))) (mergeSort(snd (splitlist xs)))
