{- HLINT ignore "Use foldr" -}
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

isPalindrone :: String -> Bool
isPalindrone x = removeSpaces x == myReverse (removeSpaces x)

myReverse :: String -> String
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

removeSpaces :: String -> String
removeSpaces xs = [x | x <- xs, x /= ' ']