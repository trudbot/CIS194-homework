fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)
  
-- 将上述两个函数重写， 使其更满足wholemeal编程风格
fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

testList :: [Integer]
testList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

main = do
  print (fun1 testList == fun1' testList)
  print (fun2 10 == fun2' 10)
  print (fun2 10000 == fun2' 10000)