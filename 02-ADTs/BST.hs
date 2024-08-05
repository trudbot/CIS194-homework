newtype NodeData = NodeData Int
  deriving (Show, Eq)

data BST = Leaf | Node BST NodeData BST
  deriving (Show, Eq)

data CompareResult = LESS | EQUAL | GREATER
  deriving (Show, Eq)

compareNodeData :: NodeData -> NodeData -> CompareResult
compareNodeData (NodeData x) (NodeData y)
  | x == y = EQUAL
  | x < y = LESS
  | x > y = GREATER

-- 在二叉搜索树中插入一个元素
insert :: BST -> NodeData -> BST
insert Leaf nodeData = Node Leaf nodeData Leaf
insert (Node left nodeDataRoot right) nodeData = case compareNodeData nodeData nodeDataRoot of
  EQUAL -> Node left nodeData right
  LESS -> Node (insert left nodeData) nodeDataRoot right
  GREATER -> Node left nodeDataRoot (insert right nodeData)

-- 中序遍历
inOrder :: BST -> [NodeData]
inOrder Leaf = []
inOrder (Node left nodeData right) = inOrder left ++ [nodeData] ++ inOrder right

-- 从列表构建二叉搜索树
build :: [NodeData] -> BST
build = foldl insert Leaf