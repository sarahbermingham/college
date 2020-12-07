evalPoly :: Int -> [Int] -> Int
evalPoly x a = evalPolyR x 0 a

evalPolyR :: Int -> Int -> [Int] -> Int
evalPolyR x power [] = 0
evalPolyR x power [h] = ((x ^ power) * h)
evalPolyR x power (h:t) = ((x ^ power) * h) + evalPolyR x (power + 1) t 



