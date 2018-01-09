-- Binary tree
data BinaryTree a =
    Leaf | Node
    { left:: BinaryTree a
    , index:: a
    , right:: BinaryTree a
    }
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' node Leaf = Node Leaf node Leaf
insert' node (Node left index right)
    | node == index = Node left index right
    | node < index = Node(insert' node left) index right
    | node > index = Node left index (insert' node right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left index right) =
    Node (mapTree f left) (f index) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
    -- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

-- convert to lists
preorder :: Ord a => BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left index right) =
    [index] ++ preorder left ++ preorder right

inorder :: Ord a => BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left index right) =
    preorder left ++ [index] ++ preorder right

postorder :: Ord a => BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left index right) =
    preorder left ++ preorder right ++ [index]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 4 Leaf) 3 (Node Leaf 5 Leaf))

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3, 4, 5]
    then putStrLn "Preorder fine!"
    else putStrLn "preorder failed check"

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3, 4, 5]
    then putStrLn "Inorder fine!"
    else putStrLn "inorder failed check"

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 4, 5, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder

--folding tree
foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree _ Leaf = Leaf
foldTree f (Node left index right) =
    

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt = undefined

mapOkay' =
    if mapTree' (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"
