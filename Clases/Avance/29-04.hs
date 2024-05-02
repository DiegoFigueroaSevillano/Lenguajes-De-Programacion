data MyList a = Empty | Cons a (MyList a) deriving (Show, Eq, Ord)

lista :: MyList Int
lista = Cons 5 (Cons 4 (Cons 5 Empty))

ordenar :: [Int] 
ordenar = map (+2) [1,2,3]

