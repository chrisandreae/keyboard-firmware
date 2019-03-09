module Errors where
import BasicTypes
import ErrorState (FailError, failError)

-- Error handling

type ErrorContext = String

data CompileError = Redefine String
                  | MapLookupError String String -- name, index
                  | GraphLookupError Int
                  | DefaultError String
                  | InternalError String
                  | ParsingError String
                  | TypeError Type Type ErrorContext -- reqType actualType
                  | ReturnTypeError Type Type ErrorContext -- reqType actualType
                  | NeedsValueError ErrorContext -- void expression in value context
                  | LValueError ErrorContext
                  | LiteralError Type Int
                  | UndefinedVarError String ErrorContext
                  | UndefinedMethodError String ErrorContext
                  | MethodArgsError String Int ErrorContext
                  | VoidCastError String
                  | NoReturnError ErrorContext
                  | NoMainError

instance FailError CompileError where
  failError = InternalError

showHdr :: Int -> String -> ShowS -> ShowS
showHdr d s sr = showParen (d>10) $ showString s . showChar '\n' . sr

instance Show CompileError where
  showsPrec d (Redefine s)              = showHdr d "Redefine" $ showString s
  showsPrec d (MapLookupError n idx)    = showHdr d "MapLookupError" $
    showString n . showChar '\n' . showString idx
  showsPrec d (GraphLookupError x)      = showHdr d "GraphLookupError" $ shows x
  showsPrec d (DefaultError s)          = showHdr d "DefaultError" $ showString s
  showsPrec d (InternalError s)         = showHdr d "InternalError" $ showString s
  showsPrec d (ParsingError s)          = showHdr d "ParsingError" $ showString s
  showsPrec d (TypeError reqT actT ctx) = showHdr d "TypeError" $
    showString "req=" . shows reqT . showString "; act=" . shows actT . showChar '\n' . showString ctx
  showsPrec d (ReturnTypeError reqT actT ctx) = showHdr d "ReturnTypeError " $
    showString "req=" . shows reqT . showString "; act=" . shows actT . showChar '\n' . showString ctx
  showsPrec d (NeedsValueError ctx)     = showHdr d "NeedsValueError" $ showString ctx
  showsPrec d (LValueError ctx)         = showHdr d "LValueError" $ showString ctx
  showsPrec d (LiteralError t x)        = showHdr d "LiteralError" $ shows t . showChar ' ' . shows x
  showsPrec d (UndefinedVarError s ctx) = showHdr d "UndefinedVarError" $
    showString s . showChar '\n' . showString ctx
  showsPrec d (UndefinedMethodError s ctx) = showHdr d "UndefinedMethodError" $
    showString s . showChar '\n' . showString ctx
  showsPrec d (MethodArgsError s x ctx) = showHdr d "MethodArgsError" $
    showString s . showChar '\n' . shows x . showChar '\n' . showString ctx
  showsPrec d (VoidCastError s)         = showHdr d "VoidCastError" $ showString s
  showsPrec d (NoReturnError ctx)       = showHdr d "NoReturnError" $ showString ctx
  showsPrec d (NoMainError)             = showHdr d "NoReturnError" $ showString "(missing function main)"


type ThrowsError = Either CompileError

maybeToError :: e -> Maybe a -> Either e a
maybeToError _ (Just x)  = Right x
maybeToError c (Nothing) = Left c


isError :: ThrowsError a -> Bool
isError (Right _) = False
isError (Left _) = True
