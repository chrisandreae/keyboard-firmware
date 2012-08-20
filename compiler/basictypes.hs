module BasicTypes where

type Ident = String

data Type = Byte
          | Short
          | Void
          deriving (Ord, Eq) -- if t1 < t2, t1 can become t2 without a cast

instance Show Type where
  show Byte  = "byte"
  show Short = "short"
  show Void  = "void"
