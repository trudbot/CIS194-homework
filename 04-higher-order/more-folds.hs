xor' :: Bool -> Bool -> Bool
xor' a b = (a && not b) || (not a && b)

-- 对Bool列表进行异或操作
xor :: [Bool] -> Bool
xor = foldr xor' False

-- 使用fold实现map
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- 使用foldr 实现 foldl
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

main = do
  print (xor [False, True, False])
  print (xor [False, True, False, True, False])
