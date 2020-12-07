num :: Eq a => a -> [a] -> Int

num n [] = 0
num n (h:t)
    | n == h = 1 + num n t
    | otherwise = num n t