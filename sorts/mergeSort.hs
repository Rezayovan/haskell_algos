

mergeSort :: Ord a => [a] -> [a]
mergeSort [x,y] = if x < y then [x, y] else [y, x]
mergeSort [x] = [x]
mergeSort xs = f (mergeSort (take half xs)) (mergeSort (drop half xs))
    where
        half = length xs `div` 2
        f :: Ord a => [a] -> [a] -> [a] -- for clarification
        f (x:xs) (y:ys) = if x < y then x : f xs (y:ys) else y : f (x:xs) ys
        f xs [] = xs
        f [] ys = ys



