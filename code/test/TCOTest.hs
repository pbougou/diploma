module TCOTest where

import SpecCentre
import Data.Tuple.HT(fst3)
import System.IO.Unsafe

tcoSpec :: Spec
tcoSpec = 
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

    in  describe "Test tail calls" $ do

        let     factByVal = unsafePerformIO $ readFile factByValPath
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
        
        context (show $ spotTCs p) $
            it "should be 3628800" $
                fst3 (run (spotTCs p)) `shouldBe` 3628800
