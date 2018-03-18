{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ErrorState where
import Control.Monad.Except
import Control.Monad.State

newtype ThrowsState et st a =
  ThrowsState { unThrowsState :: ExceptT et (State st) a }
  deriving (Functor, Applicative, Monad, MonadState st, MonadError et)

runThrowsState :: st -> ThrowsState et st a -> ((Either et a), st)
runThrowsState st x = runState (runExceptT $ unThrowsState x) st

execThrowsState :: st -> ThrowsState et st a -> Either et st
execThrowsState st x = let (result, resultState) = runThrowsState st x
                       in either (Left) (const $ Right resultState) result

{- -- Can't make this work because it isn't itself a transformer? (*->*)->*->* ?
instance MonadTrans (ThrowsState et) where
  lift m = ThrowsState $ ErrorT (m >>= return . Right)
-}

-- We need to be able to lift a (Either et b) (the 'bottom' part of
-- the ExceptT monad transformer) into the (ThrowsState et st b) in the
-- same way we want to be able to lift a (State st b) into the top
-- part using lift (lift m would be ThrowsState $ ExceptT (m >>= return
-- . Right) We could also do this by deconstructing the Either and
-- then using our functions from Monad and MonadError: (liftThrows =
-- either throwError return)
liftThrows :: Either et b -> ThrowsState et st b
liftThrows = ThrowsState . ExceptT . return

-- Something like mplus but without need to introduce mzero/empty
-- (i.e. Monoid's mempty over CompileError). Especially when code
-- used mplus more like alternative and not like an error container.
-- Note that orTry does not thread the inner state monad through
-- the falied alternatives.
infixr 3 `orTry`
orTry :: ThrowsState et st a -> ThrowsState et st a -> ThrowsState et st a
orTry option1 option2 =
  ThrowsState $ ExceptT $ state $ \s0 ->
    case runThrowsState s0 option1 of
      rv@((Right _),_) -> rv
      _ -> runThrowsState s0 option2

guardError :: et -> Bool -> ThrowsState et st ()
guardError _ True = return ()
guardError e False = throwError e
