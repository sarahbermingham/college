elemSorted :: Ord a => a -> [a] -> Bool

elemSorted n [] = False
elemSorted n (h:t)
    | n == h = True
    | n < h = False
    | otherwise = elemSorted n t