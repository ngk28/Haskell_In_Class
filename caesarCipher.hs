import Control.Arrow (ArrowLoop (loop))
import Data.Char

upperChar :: String -> [Char]
upperChar xs = [toUpper x | x <- xs]

shiftByN :: String -> Int -> [Int]
shiftByN xs n = [ord x + mod n 26 | x <- upperChar xs]

loopInt :: Int -> Int
loopInt x
  | x >= 65 + 26 = x - 26
  | x < 65 = x + 26
  | otherwise = x

loopAround :: [Int] -> [Int]
loopAround xs = [loopInt x | x <- xs]

encryptString :: String -> Int -> String
encryptString s n = [chr x | x <- loopAround (shiftByN s n)]