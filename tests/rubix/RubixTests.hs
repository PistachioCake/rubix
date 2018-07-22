module rubixTests where

import Rubix

import Test.Hspec
import Text.Printf (printf)

isSolved = shouldBe solved 


main = hspec $ do
  describe "Test moves and their inverses" $ do
    let 
      testInverse :: Alg -> Alg -> Expectation
      testInverse a b = solved `shouldBe` $ (solved `apply` a) `apply` b
      inverses = [(f, f'), (r, r'), (l, l'), (u, u'), (d, d'), (b, b'), (f2, f2), (r2, r2), (l2, l2), (u2, u2), (d2, d2), (b2, b2)]
    it (printf "%s and %s" 




hspec :: Spec -> IO ()
describe :: String -> SpecWith a -> SpecWith a
it :: Example a => String -> a -> SpecWith (Arg a)


