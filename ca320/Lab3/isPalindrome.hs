isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome a = (head a == last a) && (isPalindrome (tail (init(a))))