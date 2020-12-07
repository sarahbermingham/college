repAll :: Eq a => a -> a -> [a] -> [a]
repAll n r [] = []
repAll n r (h:t)
    | n == h = r : repAll n r t
    | otherwise = h : repAll n r t



repFirst :: Eq a => a -> a -> [a] -> [a] 
repFirst n r [] = []
repFirst n r (h:t)
    | n == h = r : t
    | otherwise = h : repFirst n r t