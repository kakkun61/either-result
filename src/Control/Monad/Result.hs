{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- | This monad transformer extends a monad with the ability to fail.
module Control.Monad.Result
  ( -- * The class
    MonadError (throwError, catchError)
  , liftResult
    -- * The ResultT monad transformer
  , type ResultT (ResultT)
  , runResultT
  , mapResultT
    -- * The Result monad
  , type Result
  , pattern Result
  , runResult
  , pattern Error
  , pattern Success
  , result
  , fromEither
  , toEither
  , fromSuccess
  , toMonadFail
    -- * Re-exports
  , module Control.Monad
  , module Control.Monad.Fix
  , module Control.Monad.Trans
  ) where

import           Control.Monad.Error.Class         (MonadError (catchError, throwError))
import           Control.Monad.Trans.Except.Result (Result, ResultT)
import           Control.Monad.Trans.Result        (pattern Error, pattern Result, pattern ResultT, pattern Success,
                                                    catchE, fromEither, fromSuccess, mapResultT, result, runResult,
                                                    runResultT, throwE, toEither, toMonadFail)

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans

instance Monad m => MonadError String (ResultT m) where
  throwError = throwE
  {-# INLINE throwError #-}

  catchError = catchE
  {-# INLINE catchError #-}

-- | Lift 'Result' into 'MonadError'.
liftResult :: MonadError String m => Result a -> m a
liftResult = result throwError pure
{-# INLINE liftResult #-}
