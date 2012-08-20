module Errors where
import Control.Monad.Error
import BasicTypes

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
                  deriving Show

instance Error CompileError where
  noMsg = DefaultError "An error has occurred"
  strMsg = DefaultError

type ThrowsError = Either CompileError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

extractError :: ThrowsError a -> CompileError
extractError (Left val) = val

maybeToError :: Error e => e -> Maybe a -> Either e a
maybeToError _ (Just x)  = Right x
maybeToError c (Nothing) = Left c

errorToMaybe :: ThrowsError a -> Maybe a
errorToMaybe (Right val) = Just val
errorToMaybe (Left _)    = Nothing

isError :: ThrowsError a -> Bool
isError (Right _) = False
isError (Left _) = True
