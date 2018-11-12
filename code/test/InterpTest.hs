module InterpTest where

import SpecCentre
import Data.Tuple.HT(fst3)
import System.IO.Unsafe


spec :: Spec
spec = 
    let rootPath = "./examples/"

        byValPath = rootPath ++ "byval/"
        byNamePath = rootPath ++ "byname/"
        lazyPath = rootPath ++ "lazy/"

        -- Only strict arguments
        factByValPath = byValPath ++ "fact_byval"
        gcdByValPath = byValPath ++ "gcd_byval"
        factTCbyValPath = byValPath ++ "tailrec_fact_byval"

        -- Only CBN
        factByNamePath = byNamePath ++ "fact_byname"
        gcdByNamePath = byNamePath ++ "gcd_byname"

        -- Only lazy args
        factLazyPath = lazyPath ++ "fact_lazy"
        gcdLazyPath = lazyPath ++ "gcd_lazy"
        lazy1Path = lazyPath ++ "lazy1"

    in  describe "Test results, no optimization" $ do
            let factByVal = unsafePerformIO $ readFile factByValPath
                p = unsafePerformIO $ parseProgram factByVal

                gcdByVal = unsafePerformIO $ readFile gcdByValPath
                astGcd   = unsafePerformIO $ parseProgram gcdByVal

                factTCbyVal = unsafePerformIO $ readFile factTCbyValPath
                astFactTC = unsafePerformIO $ parseProgram factTCbyVal

                -- CBN
                factByName = unsafePerformIO $ readFile factByNamePath
                astFactBN = unsafePerformIO $ parseProgram factByName

                gcdByName = unsafePerformIO $ readFile gcdByNamePath
                astGcdBN = unsafePerformIO $ parseProgram gcdByName

                -- Lazy 
                factLazy = unsafePerformIO $ readFile factLazyPath
                astFactLazy = unsafePerformIO $ parseProgram factLazy

                gcdLazy = unsafePerformIO $ readFile gcdLazyPath
                astGcdLazy = unsafePerformIO $ parseProgram gcdLazy

                lazy1 = unsafePerformIO $ readFile lazy1Path
                astLazy1 = unsafePerformIO $ parseProgram lazy1

            context "fun main = 1" $
                it "should be 1" $ 
                    fromValue (fst3 (run [Fun "main" [] (EInt 1)])) `shouldBe` 1

            context "fun main = if 1 then 42 else 0" $
                it "should be 42" $
                    fromValue (fst3 (run [Fun "main" [] (Eif (EInt 1) (EInt 42) (EInt 0))])) `shouldBe` 42
            

            context factByVal $
                it "should be 3628800" $
                    fromValue (fst3 (run p)) `shouldBe` 3628800

            context gcdByVal $
                it "should be 7" $
                    fromValue (fst3 (run astGcd)) `shouldBe` 7

            context factTCbyVal $
                it "should be 3628800" $
                    fromValue (fst3 (run astFactTC)) `shouldBe` 3628800

            context factByName $
                it "should be 720" $
                    fromValue (fst3 (run astFactBN)) `shouldBe` 720

            context gcdByName $
                it "should be 7" $
                    fromValue (fst3 (run astGcdBN)) `shouldBe` 7

            context factLazy $ 
                it "should be 6" $
                    fromValue (fst3 (run astFactLazy)) `shouldBe` 6
            
            context gcdLazy $
                it "should be 7" $
                    fromValue (fst3 (run astGcdLazy)) `shouldBe` 7

            context lazy1 $
                it "should be 7" $
                    fromValue (fst3 (run astLazy1)) `shouldBe` 7

            

            



