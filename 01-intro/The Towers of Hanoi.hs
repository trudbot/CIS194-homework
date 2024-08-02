type Peg = String
type Move = (Peg, Peg)
-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- 第一个柱子中盘子的数量， 三个柱子的名字， 返回移动的步骤
-- 目的： 将第一个柱子中的盘子移动到第二个柱子上， 使用第三个柱子做辅助
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

main = do
  print (hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")])