chain::(Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 +1)
numLongChains :: Int
numLongChains = length $ filter (>15) $ map (length.chain) [1..100]
