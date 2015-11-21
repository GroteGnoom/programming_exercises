module General where

zip = List.map2 (,)

allCombinations : List a -> List b -> List (a,b)
allCombinations la lb =
    List.concat (List.map (allCombinationsHelper la) lb)

allCombinationsHelper : List a -> b -> List (a,b)
allCombinationsHelper la b =
    List.map (makeTuple b) la 

makeTuple : a -> b -> (b,a)
makeTuple a b = (b,a) 

listRotate : List a -> List a
listRotate list =
    List.append (Maybe.withDefault [] (List.tail list)) (List.take 1 list)