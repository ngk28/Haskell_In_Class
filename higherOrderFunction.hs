import Data.Char (isAlpha, isDigit)

square :: (Num a) => a -> a
square x = x * x

squareList :: (Num a) => [a] -> [a]
squareList = map square

isEven :: Int -> Bool
isEven x = mod x 2 == 0

filterEven :: [Int] -> [Int]
filterEven = filter isEven

mySum :: (Num a) => [a] -> a
mySum = foldr (+) 0

mySquareSum :: (Num a) => [a] -> a
mySquareSum xs = foldr (+) 0 (map square xs)