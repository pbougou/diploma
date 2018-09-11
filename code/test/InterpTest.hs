module InterpTest where

import SpecCentre
import Data.Tuple.HT(fst3)
import System.IO.Unsafe


spec :: Spec
spec = 
    let rootPath = "./examples/"
        byValPath = rootPath ++ "byval/"
        factByValPath = byValPath ++ "fact_byval"
    in  describe "fact by value" $ do
            let factByVal = unsafePerformIO $ readFile factByValPath
                p = unsafePerformIO $ parseProgram factByVal

            context "fun main = 1" $
                it "should be 1" $ 
                    fst3 (run [Fun "main" [] (EInt 1)]) `shouldBe` 1

            context "fun main = if 1 then 42 else 0" $
                it "should be 42" $
                    fst3 (run [Fun "main" [] (Eif (EInt 1) (EInt 42) (EInt 0))]) `shouldBe` 42
            

            context factByVal $
                it "should be 3628800" $
                    fst3 (run p) `shouldBe` 3628800

            



