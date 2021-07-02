module Main where
import Test.Hspec ( hspec, describe, it )
import Test.QuickCheck (property)

main :: IO ()
main = hspec $ do
  describe "<method you would like to test>" $ 
    it "<property/assumption you would like to test>" $ 
       property True
