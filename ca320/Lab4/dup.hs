dup :: Eq a => [a] -> Bool

dup [] = False
dup [a] = False
dup (h:t)
    | elem h t = True
    | otherwise = dup t

dupSorted :: Eq a => [a] -> Bool

dupSorted [] = False
dupSorted [a] = False
dupSorted (h:t)
    | h == head t = True
    | otherwise = dupSorted t


nubSorted :: Eq a => [a] -> [a]

nubSorted [] = []
nubSorted [a] = [a]
nubSorted (h:t)
    | h == head t = nubSorted (t)
    | otherwise = h:nubSorted(t)