module Golf where
import Data.List (unfoldr, group, sort, intercalate)


splitByLength :: Int -> [a] -> [[a]]
splitByLength n = unfoldr (\xs -> if length xs < n then Nothing else Just (splitAt n xs))

-- 从1开始， 每隔n个元素取一个元素, 直到n > 列表的长度
-- 思路是先将数组按长度为n分组(不足n的尾巴扔掉)，然后取每个分组的最后一个元素
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
skips :: [a] -> [[a]]
skips xs = unfoldr (\(n, ys) -> if n > length ys then Nothing else Just (map last (splitByLength n ys), (n + 1, ys))) (1, xs)

-- 查找出列表中所有满足 严格大于前一个元素和后一个元素的元素
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : xs) = if y > x && y > z then y : localMaxima (y : z : xs) else localMaxima (y : z : xs)
localMaxima _ = []

-- 统计列表中每个元素出现的次数， 返回一个元组， 第一个元素是元素和出现次数的列表， 第二个元素是出现次数的最大值
countOccurrences :: [Int] -> ([(Int, Int)], Int)
countOccurrences xs = 
  let occurrences = map (\g -> (head g, length g)) . group . sort $ xs
      maxLength = maximum (map snd occurrences)
  in (occurrences, maxLength)

-- 得到当前行需要画的数
getDrawData :: Int -> [(Int, Int)] -> [Int]
getDrawData level = map fst . filter (\(_, y) -> y >= level)

-- 输入当前行需要画的数， 画出当前行
drawLine :: [Int] -> String
drawLine xs = map (\x -> if x `elem` xs then '*' else ' ') [0..9]

-- 输入数组， 统计每个数字都出现次数， 画出直方图
histogram :: [Int] -> String
histogram xs = intercalate "\n" (unfoldr (\(level, cnt) ->
    if level == 0 then Nothing else Just (drawLine (getDrawData level cnt), (level - 1, cnt))
  ) (snd (countOccurrences xs), fst (countOccurrences xs))) ++ "\n==========\n0123456789\n"

main = do
  print (splitByLength 11 [1])
  print (skips "ABCD")
  print (skips "hello!")
  print (skips [1])
  print (skips [True,False])
  print (skips [] :: [[Int]])

  print (localMaxima [2,9,5,6,1])
  print (localMaxima [2,3,4,1,5])
  print (localMaxima [1,2,3,4,5])

  putStr (histogram [2, 2 ,5])