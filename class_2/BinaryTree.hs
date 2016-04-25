import Data.Maybe

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
binaryTreeToString tree@(BinaryTreeNode a left right)
    | isLeave = aAsString
    | otherwise = aAsString
                    ++ "(" ++ leftAsString
                    ++ ", " ++ rightAsString
                    ++ ")"
    where   leftAsString = binaryTreeToString left
            rightAsString = binaryTreeToString right
            isLeave = binaryTreeIsLeave tree
            aAsString = show a

binaryTreeIsBalanced :: BinaryTree a -> Bool
binaryTreeIsBalanced tree = balanced
    where (_, balanced) = binaryTreeBalanceData tree

binaryTreeBalanceData :: BinaryTree a -> (Int, Bool)
binaryTreeBalanceData EmptyBinaryTree = (0, True)
binaryTreeBalanceData (BinaryTreeNode _ left right) =
    let (leftHeight, _) = binaryTreeBalanceData left
        (rightHeight, _) = binaryTreeBalanceData right
        treeBalanced = abs (leftHeight - rightHeight) <= 1
        treeHeight = max leftHeight rightHeight + 1
    in (treeHeight, treeBalanced)

binaryTreeLeaves :: (Eq a) => BinaryTree a -> [a]
binaryTreeLeaves EmptyBinaryTree = []
binaryTreeLeaves tree@(BinaryTreeNode a left right)
    | isLeave = [a]
    | otherwise = leftLeaves ++ rightLeaves
    where   leftLeaves = binaryTreeLeaves left
            rightLeaves = binaryTreeLeaves right
            isLeave = binaryTreeIsLeave tree

binaryTreeNodesNo :: BinaryTree a -> Int
binaryTreeNodesNo EmptyBinaryTree = 0
binaryTreeNodesNo (BinaryTreeNode _ left right)
    = 1 + leftNodesNo + rightNodesNo
    where   leftNodesNo = binaryTreeNodesNo left
            rightNodesNo = binaryTreeNodesNo right

binaryTreeSum :: (Num a) => BinaryTree a -> Maybe a
binaryTreeSum EmptyBinaryTree = Nothing
binaryTreeSum (BinaryTreeNode a left right)
    = Just allPureValuesSum
    where   leftSum = binaryTreeSum left
            rightSum = binaryTreeSum right
            allMaybeValues = [Just a,leftSum,rightSum]
            allPureValues = catMaybes allMaybeValues
            allPureValuesSum = sum allPureValues


binaryTreeIsLeave :: (Eq a) => BinaryTree a -> Bool
binaryTreeIsLeave (BinaryTreeNode _ left right)
    = left == EmptyBinaryTree && right == EmptyBinaryTree
