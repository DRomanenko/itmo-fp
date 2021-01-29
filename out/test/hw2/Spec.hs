import AllTests
import Test.Tasty

main :: IO ()
main = hTestTree >>= \unitTests -> defaultMain (testGroup "hw2-DRomanenko-AllTests" [unitTests])
