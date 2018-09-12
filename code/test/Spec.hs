import Test.Hspec
import InterpTest
import TCOTest

main :: IO ()
main = do
    hspec spec
    hspec tcoSpec
