module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go  n   d count
          | n < d = (count, n)
          | otherwise =
              go (n - d) d (count + 1)

mul :: (Integral a) => a -> a -> a
mul x 1 = x
mul x y = x + mul x (y - 1)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is\
       \ 4 remainder 2" $ do
       dividedBy 22 5 `shouldBe` (4, 2)
    it "11 multiplied by 2 is 22" $ do
      mul 11 2 `shouldBe` 22
    it "x + 1 is always\
       \ greater than x" $ do
       property $ \x -> x + 1 > (x :: Int)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']
