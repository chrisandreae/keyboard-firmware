{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ErrorState where
import Control.Monad.Except
import Control.Monad.State

newtype ThrowsState et st a = ThrowsState { unThrowsState :: ExceptT et (State st) a }
                  deriving (Applicative, Monad, MonadState st, MonadError et, Functor)

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
-- the ErrorT monad transformer) into the (ThrowsState et st b) in the
-- same way we want to be able to lift a (State st b) into the top
-- part using lift (lift m would be ThrowsState $ ErrorT (m >>= return
-- . Right) We could also do this by deconstructing the Either and
-- then using our functions from Monad and MonadError: (liftThrows =
-- either throwError return)
liftThrows :: Either et b -> ThrowsState et st b
liftThrows = ThrowsState . ExceptT . return

guardError :: et -> Bool -> ThrowsState et st ()
guardError _ True = return ()
guardError e False = throwError e
