module PPrint
    ( someFunc
    ) where

import Grammar
import Text.Read

someFunc :: IO ()
someFunc = putStrLn "someFunc"

showFormals formals = foldr (\x y -> x ++ " " ++ y) " " formals


instance Show Expr where
  showsPrec p (Call x l) =
      ("call " ++) . (x ++) . (" " ++) .showsPrec 0 l
--        (
--          (foldr (\x y -> (showsPrec p x) . (++ " ") . (showsPrec y)) "" l) ++
--        )
  showsPrec p (Eif x y z) = ("if " ++) . showsPrec 0 x .
                            (" then " ++) . showsPrec 0 y .
                            (" else " ++) . showsPrec 0 z
  showsPrec p (EVar x)   = (x ++)
  showsPrec p (EInt x)   = (show x ++)
  showsPrec p (EAdd x y) =
      showParen (p > 0) $
          showsPrec 2 x . (" + " ++) . showsPrec 2 y
  showsPrec p (ESub x y) =
      showParen (p > 0) $
          showsPrec 2 x . (" - " ++) . showsPrec 2 y
  showsPrec p (EMul x y) = showsPrec 1 x . (" * " ++) . showsPrec 1 y
  showsPrec p (EDiv x y) = showsPrec 1 x . (" / " ++) . showsPrec 1 y
  showsPrec p (EMod x y) = showsPrec 1 x . (" % " ++) . showsPrec 1 y


