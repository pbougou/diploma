module InterpTest where

import SpecCentre

spec :: Spec
spec = 
    describe "fact by value" $ do
        context "fun main = 1" $
            it "should be 1" $ 
                run (Fun "main" [] (EInt 1)) `shouldBe` 1

        context "fun main = if 1 then 42 else 0" $
            it "should be 42" $
                run (Fun "main" [] (Eif (EInt 1) (EInt 42) (EInt 0))) `shouldBe` 42
