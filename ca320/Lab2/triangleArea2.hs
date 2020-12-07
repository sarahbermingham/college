triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c 
   | isTriangle a b c == True = sqrt(s * (s - a) * (s - b) * (s - c))
   | otherwise = error "Not a triangle!"
   where s = (a + b + c) / 2

-- sqrt formula from labsheet

isTriangle :: Float -> Float -> Float -> Bool
isTriangle x y z
   | x + z > y && y + z > x && x + y > z = True
   | otherwise = False
