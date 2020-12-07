sumPoly :: [Int] -> [Int] -> [Int]

sumPoly [] [] = []
sumPoly [x] [y] = (x + y) : []
sumPoly [x] [] = (x) : []
sumPoly [] [x] = (x) : []
sumPoly (h1:t1) (h2:t2) = (h1 + h2): sumPoly t1 t2