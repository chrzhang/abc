data Tree a = Null | Node a (Tree a) (Tree a)

inOrderList :: Tree a -> [a]
inOrderList Null = []
inOrderList (Node item left right) =
    inOrderList(left) ++ [item] ++ inOrderList(right)

countNodesIn :: Tree a -> Int
countNodesIn Null = 0
countNodesIn (Node _ left right) =
    1 + countNodesIn left + countNodesIn right

getInfTree :: Int -> Tree Int
getInfTree root = Node root (getInfTree (root - 1)) (getInfTree (root + 1))

main = do
    let t = Node 3 (Node 2 Null Null) (Node 5 (Node 4 Null Null) Null)
    putStrLn(show (inOrderList t))
    putStrLn("Node count: " ++ show (countNodesIn t))
    -- Would run out of memory
    -- let infiniteTree = getInfTree 0
    -- putStrLn(show (inOrderList infiniteTree))
