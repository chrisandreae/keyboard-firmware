module Errors where
import Control.Monad.Except
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


noMsg :: CompileError
noMsg = DefaultError "An error has occured"

type ThrowsError = Either CompileError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

extractError :: ThrowsError a -> CompileError
extractError (Left val) = val

maybeToError :: (MonadError e m) => e -> Maybe a -> m a
maybeToError _ (Just x)  = return x
maybeToError c (Nothing) = throwError c

errorToMaybe :: ThrowsError a -> Maybe a
errorToMaybe (Right val) = Just val
errorToMaybe (Left _)    = Nothing

isError :: ThrowsError a -> Bool
isError (Right _) = False
isError (Left _) = True
