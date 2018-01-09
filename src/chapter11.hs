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
