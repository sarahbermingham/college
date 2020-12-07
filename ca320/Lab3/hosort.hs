data Tree a = Null
	| Node a (Tree a) (Tree a)
	deriving (Read, Show)

addNode :: Ord a => a -> Tree a -> Tree a
addNode t Null = Node t Null Null
addNode t (Node n left right)
	| t < n = Node n (addNode t left) right
	| otherwise = Node n left (addNode t right)

makeTree :: Ord a => [a] -> Tree a
makeTree [] = Null 
makeTree (h:t) = addNode h (makeTree t)

inOrder :: Tree a -> [a]
inOrder Null = []
inOrder (Node n left right) = (inOrder left) ++ [n] ++ (inOrder right)

mpSort :: Ord a => [a] -> [a]
mpSort a = inOrder (makeTree a)


hoInsert :: Ord a => (a -> a -> Bool) -> a -> Tree a -> Tree a
hoInsert op item Null = Node item Null Null
hoInsert op item (Node n left right)
	| item `op` n = Node n (hoInsert op item left) right
	| otherwise = Node n left (hoInsert op item right)

hoMakeTree :: Ord a => (a -> a -> Bool) -> [a] -> Tree a
hoMakeTree _ [] = Null
hoMakeTree _ [x] = Node x (Null) (Null)
hoMakeTree op (x:xs) = hoInsert op x (hoMakeTree op xs)

hosort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
hosort op list = inOrder(hoMakeTree op list)