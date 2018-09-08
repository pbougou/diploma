module PPrint where

import Grammar
import Text.Read


instance Show Expr where
  showList [] = ("" ++)
  showList (l : ls) = showsPrec 1 l . (" " ++) . showList ls
  showsPrec p (Call x l) =
      ("call " ++) . (x ++) . (" " ++) .showsPrec 0 l
  showsPrec p (Eif x y z) =
    showParen (p > 0) $
        ("if " ++) . showsPrec 0 x .
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

instance Show FDef where
  showList [] = ("" ++)
  showList (l : ls) = showsPrec 0 l .(";\n" ++) .showList ls
  showsPrec p (Fun x l e) = ("fun " ++) . (x ++) . (" " ++) . (l' ++) .
                            ("=\n    " ++) . showsPrec 0 e
     where l' = showFormals l
           showFormals =
             foldr (\x y -> let ch = case snd x of { CBV  -> "!"; CBN  -> "#"; Lazy -> "" }
             in ch ++ fst x ++ " " ++ y) ""

instance Show Type where
  showsPrec p t = case t of CBV  -> ("! " ++)
                            CBN  -> ("# " ++)
                            Lazy -> ("" ++)
