-- 将一个元素加入列表的末尾
append :: [Integer] -> Integer -> [Integer]
append [] x = [x]
append (x:xs) y = x : append xs y

-- 反转列表
listRev :: [Integer] -> [Integer]
listRev [] = []
listRev (x : xs) = append (listRev xs) x

-- toDigitsRev 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- toDigits 1234 == [1,2,3,4]
-- toDigits 0 == []
-- toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits n = listRev (toDigitsRev n)

-- 正序的每隔两个元素乘以2， 比反序更好处理
doubleEveryOtherFromStart :: [Integer] -> [Integer]
doubleEveryOtherFromStart [] = []
doubleEveryOtherFromStart [x] = [x]
doubleEveryOtherFromStart (x : y : zs) = x : (y * 2) : doubleEveryOtherFromStart zs

-- 反序的每隔两个元素乘以2
-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] == [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = listRev (doubleEveryOtherFromStart (listRev list))

getNumberDigitsSum :: Integer -> Integer
getNumberDigitsSum n
  | n <= 0 = 0
  | otherwise = (n `mod` 10) + getNumberDigitsSum (n `div` 10)

-- 求列表中每个数字都数位和之和
-- sumDigits [16,7,12,5] == 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = getNumberDigitsSum x + sumDigits xs

-- 校验银行卡号
-- validate 4012888888881881 == True
-- validate 4012888888881882 = False
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

main = do
  print (toDigits (-17) == [])
  print (toDigits 1234 == [1,2,3,4])
  print (toDigits 0 == [])
  print (toDigitsRev 1234 == [4,3,2,1])
  print (doubleEveryOther [8,7,6,5] == [16,7,12,5])
  print (doubleEveryOther [1,2,3] == [1,4,3])
  print (sumDigits [16,7,12,5] == 22)
  print (validate 4012888888881881)
  print (not (validate 4012888888881882))