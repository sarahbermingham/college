delAll :: Eq a => a -> [a] -> [a]
delAll n [] = []
delAll n (h:t)
    | h == n = delAll n t
    | otherwise = h: delAll n t


delFirst :: Eq a => a -> [a] -> [a]
delFirst n [] = []
delFirst n (h:t)
    | h == n = t
    | otherwise = h: delFirst n t