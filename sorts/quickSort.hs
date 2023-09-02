

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort xs = quickSort left ++ pivotValue : quickSort right
    where
        pivotValue = xs !! (length xs `div` 2)
        left  = filter (< pivotValue) xs
        right  = filter (> pivotValue) xs

