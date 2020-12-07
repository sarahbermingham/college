myProduct :: [Int] -> Int

myProduct [] = 1
myProduct (h:t) = h * myProduct t
--that multiplies together all the elements of a list of integers.

