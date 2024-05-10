
data Tree a = Leaf a | Empty |  Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where 
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf a) = Leaf (f a) 
    fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Applicative Tree where 
    pure :: a -> Tree a
    pure = Leaf 

    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    Leaf a <*> ys = fmap a ys
    (Node left f right) <*> Leaf a = Leaf (f a)
    (Node left f right) <*> (Node nLeft a nRight) = Node (left <*> nLeft) (f a) (right <*> nRight)

operationTree :: Tree (Integer -> Integer)
operationTree = Node (Leaf (+1)) (+4) (Leaf (+6))

secondOperationTree :: Tree (Integer -> Integer -> Integer)
secondOperationTree = Node (Leaf (+)) (-) (Leaf (*))

exampleTree :: Tree Integer
exampleTree = Node (Leaf 5) 6 (Leaf 8)

secondExampleTree :: Tree Integer
secondExampleTree = Node (Leaf 9) 1 (Leaf 7)

increment :: Tree Int -> Tree Int 
increment = fmap (+1)

strToIntAux :: String -> Int -> Int 
strToIntAux [] c = c
strToIntAux (x:xs) c = strToIntAux xs (c+1)

strToInt :: String -> Int
strToInt s = strToIntAux s 0

convertStr :: Tree String -> Tree Int 
convertStr = fmap strToInt 



addNode :: Ord a => Tree a -> a -> Tree a
addNode (Leaf a) b = if (b > a) then Node Empty a (Leaf b) else Node (Leaf b) a Empty
addNode Empty b = Leaf b
addNode (Node left a right) b = if (b > a) then Node left a (addNode right b) else Node (addNode left b) a right

testAddNode :: Tree Integer
testAddNode = addNode (addNode (addNode exampleTree 9) 7) 4

-- addTree :: Tree a -> Tree a -> Tree a
-- addTree a b = Leaf addNode <*> a <*> b 

addTree :: Ord a => Tree a -> Tree a -> Tree a
addTree a Empty = a
addTree a (Leaf b) = addNode a b
addTree a (Node left b right) = addNode (addTree (addTree a left) right) b

mergeString :: String -> String -> String 
mergeString a b = a ++ b

addNode' :: Ord a => Tree a -> Tree a -> Tree a 
addNode' (Leaf a) (Node leftB b rightB) = if b > a then Node Empty a  (Node leftB b rightB) else Node (Node leftB b rightB) a Empty
addNode' Empty (Node leftB b rightB)  = (Node leftB b rightB) 
addNode' (Node leftA a rightA) (Node leftB b rightB) = if b > a then Node leftA a (addNode' rightA (Node leftB b rightB)) else Node (addNode' leftA (Node leftB b rightB)) a rightA


concatNode :: Ord a => Tree a -> Tree a -> Tree a
concatNode a b = addNode' a b 

mergeTree ::Tree String -> Tree String -> Tree String
mergeTree a b = Leaf mergeString <*> a <*> b

-- concatTwoTrees :: Tree Int
-- concatTwoTrees = Leaf concatNode <*> Leaf 1 <*> Leaf 2

firstStringTree :: Tree String
firstStringTree = Node (Leaf "Hola ") "Alex " (Leaf "Fabian ")

secondStringTree :: Tree String 
secondStringTree = Node (Leaf "Mundo") "Paca" (Leaf "Cilantro")