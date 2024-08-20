data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- 获取树的高度
height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

-- 更新树的高度
updateHeight :: Tree a -> Tree a
updateHeight Leaf = Leaf
updateHeight (Node _ l v r) = Node (1 + max (height l) (height r)) l v r

-- 左旋
rotateLeft :: Tree a -> Tree a
rotateLeft Leaf = Leaf
rotateLeft cur@(Node _ _ _ Leaf) = cur
rotateLeft (Node _ l v r@(Node _ rl rv rr)) = updateHeight $ Node (height r) (Node (height l) l v rl) rv rr

-- 右旋
rotateRight :: Tree a -> Tree a
rotateRight Leaf = Leaf
rotateRight cur@(Node _ Leaf _ _) = cur
rotateRight (Node _ l@(Node _ ll lv lr) v r) = updateHeight $ Node (height l) ll lv (Node (height r) lr v r)

-- 太麻烦了，不写了
maintain :: Tree a -> Tree a
maintain Leaf = Leaf
maintain cur@(Node ch l v r)
  | height l - height r > 1 = case l of

  | height r - height l > 1 = cur


-- 将一个新结点插入到树中
-- 树 -> 新结点 -> 新树
insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf a = Node 1 Leaf a Leaf
insert cur@(Node h l v r) x
  | v < x = maintain (updateHeight (Node h l v (insert r x)))
  | v > x = maintain (updateHeight (Node h (insert l x) v r))
  | otherwise = cur

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldl insert Leaf
