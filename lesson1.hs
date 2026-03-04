double :: Int -> Int
double x = x * 2

isPalindrome :: [Char] -> Bool
isPalindrome xs = xs == reverse xs

evenTo :: Int -> [Int]
evenTo n = [x | x <- [1 .. n], even x]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], mod n x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1, n]

primesToN :: Int -> [Int]
primesToN n = [x | x <- [1 .. n], isPrime x]

characterInString :: Char -> String -> Bool
characterInString c xs = not (null [x | x <- xs, x == c])

isPerfect :: Int -> Bool
isPerfect n = sum (factors n) == 2 * n

perfectToN :: Int -> [Int]
perfectToN n = [x | x <- [1 .. n], isPerfect x]