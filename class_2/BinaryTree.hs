--export(
--    BinaryTree
--)

data BinaryTree a = EmptyBinaryTree
    | BinaryTreeNode a (BinaryTree a) (BinaryTree a)
    deriving (Show, Read, Eq)

data TraverseType = VLR | LVR | LRV | VRL | RVL | RLV


singletonBinaryTree :: a -> BinaryTree a
singletonBinaryTree x =
    BinaryTreeNode x EmptyBinaryTree EmptyBinaryTree

insertBinaryTree :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insertBinaryTree x EmptyBinaryTree = singletonBinaryTree x
insertBinaryTree x tree@(BinaryTreeNode a left right)
    | x == a    = tree
    | x < a     = BinaryTreeNode a (insertBinaryTree x left) right
    | x > a     = BinaryTreeNode a left (insertBinaryTree x right)

insertReverseBinaryTree :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insertReverseBinaryTree x EmptyBinaryTree = singletonBinaryTree x
insertReverseBinaryTree x tree@(BinaryTreeNode a left right)
    | x == a    = tree
    | x > a     = BinaryTreeNode a (insertBinaryTree x left) right
    | x < a     = BinaryTreeNode a left (insertBinaryTree x right)

binaryTreeEmpty :: BinaryTree a -> Bool
binaryTreeEmpty EmptyBinaryTree = True
binaryTreeEmpty _ = False

isBinaryTree :: (Ord a) => BinaryTree a -> Bool
isBinaryTree EmptyBinaryTree = True
isBinaryTree tree@(BinaryTreeNode a left right) =
    let properLeftNode = canBeLeftNode left tree
        properRightNode = canBeRightNode right tree
        leftIsBinaryTree = isBinaryTree left
        rightIsBinaryTree = isBinaryTree right
    in properLeftNode
        && properRightNode
        && leftIsBinaryTree
        && rightIsBinaryTree

canBeRightNode :: (Ord a) => BinaryTree a -> BinaryTree a -> Bool
canBeRightNode EmptyBinaryTree _ = True
canBeRightNode (BinaryTreeNode x _ _) (BinaryTreeNode y _ _)
    | x > y = True
    | otherwise = False

canBeLeftNode :: (Ord a) => BinaryTree a -> BinaryTree a -> Bool
canBeLeftNode EmptyBinaryTree _ = True
canBeLeftNode (BinaryTreeNode x _ _) (BinaryTreeNode y _ _)
    | x < y = True
    | otherwise = False

searchBinaryTree :: (Eq a) => a -> BinaryTree a -> Bool
searchBinaryTree _ EmptyBinaryTree = False
searchBinaryTree x (BinaryTreeNode a left right)
    | x == a = True
    | otherwise = isInLeftTree || isInRightTree
    where   isInLeftTree = searchBinaryTree x left
            isInRightTree = searchBinaryTree x right

binaryTreeToString :: (Eq a, Show a) => BinaryTree a -> String
binaryTreeToString EmptyBinaryTree = ""
binaryTreeToString (BinaryTreeNode a left right)
    | subtreeExists = aAsString
                ++ "(" ++ leftAsString
                ++ ", " ++ rightAsString
                ++ ")"
    | otherwise = aAsString
    where   leftAsString = binaryTreeToString left
            rightAsString = binaryTreeToString right
            subtreeExists = left /= EmptyBinaryTree
                || right /= EmptyBinaryTree
            aAsString = show a
