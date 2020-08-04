{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- | This monad transformer extends a monad with the ability to fail.
module Control.Monad.Trans.Except.Result
  ( -- * The Result monad
    type Result
  , pattern Result
  , runResult
    -- * The ResultT monad transformer
  , type ResultT (ResultT)
  , runResultT
  , mapResultT
    -- * Exception operations
  , throwE
  , catchE
    -- * Lifting other operations
  , liftCallCC
  , liftListen
  , liftPass
  ) where

import           Control.Monad.Signatures   (CallCC, Listen, Pass)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Except (Except, ExceptT (ExceptT), mapExceptT, runExceptT)
import qualified Control.Monad.Trans.Except as Except

import Control.Applicative        (Alternative)
import Control.Monad              (MonadPlus)
import Control.Monad.Fix          (MonadFix)
import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.Zip          (MonadZip)
import Data.Functor.Classes       (Eq1, Ord1, Read1 (liftReadPrec), Show1 (liftShowsPrec), readData, readUnaryWith,
                                   showsUnaryWith)
import Data.Functor.Contravariant (Contravariant)
import Data.Functor.Identity      (Identity)
import GHC.Generics               (Generic)

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail (fail))
#endif

-- | A result monad.
type Result = ResultT Identity

-- | Wrap @'Except' 'String' a@.
pattern Result :: Except String a -> Result a
pattern Result e = ResultT e

-- | Unwrap @'Result' a@.
runResult :: Result a -> Except String a
runResult = runResultT
{-# INLINE runResult #-}

-- | A monad transformer that is similar to 'ExceptT' except a 'MonadFail' instance.
--
-- @
-- 'fail' = 'ResultT' . 'throwE'
-- @
newtype ResultT m a =
  ResultT
    { -- | Unwrap @'ResultT' m a@.
      runResultT :: ExceptT String m a
    }
  deriving stock (Show, Read, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving newtype (Eq1, Ord1, Applicative, Alternative, Monad, MonadTrans, MonadFix, MonadZip, MonadIO, MonadPlus, Contravariant)

instance Read1 m => Read1 (ResultT m) where
  liftReadPrec rp rl =
    readData $
      readUnaryWith (liftReadPrec rp rl) "ResultT" ResultT

instance Show1 m => Show1 (ResultT m) where
  liftShowsPrec sp sl d (ResultT m) =
    showsUnaryWith (liftShowsPrec sp sl) "ResultT" d m

instance Monad m => MonadFail (ResultT m) where
    fail = throwE
    {-# INLINE fail #-}

-- | Map the unwrapped computation using the given function.
mapResultT :: (ExceptT String m a -> ExceptT String n b) -> ResultT m a -> ResultT n b
mapResultT f m = ResultT $ mapExceptT (runExceptT . f . ExceptT) $ runResultT m
{-# INLINE mapResultT #-}

-- | Signal an exception.
throwE :: Monad m => String -> ResultT m a
throwE = ResultT . Except.throwE
{-# INLINE throwE #-}

-- | Handle an exception.
catchE :: Monad m => ResultT m a -> (String -> ResultT m a) -> ResultT m a
m `catchE` h = ResultT $ runResultT m `Except.catchE` (runResultT . h)
{-# INLINE catchE #-}

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m (Either String a) (Either String b) -> CallCC (ResultT m) a b
liftCallCC callCC f = ResultT $ Except.liftCallCC callCC $ \c -> runResultT $ f $ ResultT . c
{-# INLINE liftCallCC #-}

-- | Lift a @listen@ operation to the new monad.
liftListen :: Monad m => Listen w m (Either String a) -> Listen w (ResultT m) a
liftListen listen = ResultT . Except.liftListen listen . runResultT
{-# INLINE liftListen #-}

-- | Lift a @pass@ operation to the new monad.
liftPass :: Monad m => Pass w m (Either String a) -> Pass w (ResultT m) a
liftPass pass = ResultT . Except.liftPass pass . runResultT
{-# INLINE liftPass #-}
