shortest :: [[a]]-> [a]

shortest [] = []
shortest [h] = h
shortest (h:ht:t)  
    | length h > length ht = shortest (ht : t)
    | otherwise = shortest (h : t)