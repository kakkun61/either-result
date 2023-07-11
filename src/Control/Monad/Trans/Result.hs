{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- | 'T.ResultT' interfaces with 'Result'.
module Control.Monad.Trans.Result
  ( -- * The Result monad
    type T.Result
  , pattern Result
  , runResult
  , pattern Error
  , pattern Success
  , result
  , fromEither
  , toEither
  , fromSuccess
  , toMonadFail
    -- * The ResultT monad transformer
  , type T.ResultT
  , pattern ResultT
  , runResultT
  , mapResultT
    -- * Exception operations
  , T.throwE
  , T.catchE
    -- * Lifting other operations
  , T.liftCallCC
  , T.liftListen
  , T.liftPass
  ) where

import           Control.Monad.Trans.Except        (ExceptT (ExceptT), runExcept, runExceptT)
import qualified Control.Monad.Trans.Except.Result as T
import           Data.Functor.Identity             (Identity (runIdentity))
import qualified GHC.Show                          as S
import qualified Text.Read                         as R
import           Text.Read                         (Read (readPrec))
import qualified Text.Read.Lex                     as R

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif

instance {-# OVERLAPPING #-} Show a => Show (T.Result a) where
  showsPrec d (Error e)   = showParen (S.appPrec < d) $ showString "Error " . showsPrec S.appPrec1 e
  showsPrec d (Success a) = showParen (S.appPrec < d) $ showString "Success " . showsPrec S.appPrec1 a

instance {-# OVERLAPPING #-} Read a => Read (T.Result a) where
  readPrec =
    R.parens $
      R.prec S.appPrec (
        do
          R.lift $ R.expect $ R.Ident "Error"
          e <- R.step readPrec
          pure $ Error e
      )
      R.+++
      R.prec S.appPrec (
        do
          R.lift $ R.expect $ R.Ident "Success"
          a <- R.step readPrec
          pure $ Success a
      )

instance Semigroup (T.Result a) where
  Error _ <> a = a
  a <> _       = a
  {-# INLINE (<>) #-}

instance Monoid (T.Result a) where
  mempty = Error "mempty"
  {-# INLINE mempty #-}

  mappend = (<>)
  {-# INLINE mappend #-}

-- | Wrap @'D.Result' a@.
pattern Result :: Either String a -> T.Result a
pattern Result r <- (runIdentity . runExceptT . T.runResultT -> r)
  where Result r = T.ResultT $ ExceptT $ pure r

{-# COMPLETE Result #-}

-- | Unwrap @'T.Result' a@.
runResult :: T.Result a -> Either String a
runResult = runExcept . T.runResult
{-# INLINE runResult #-}

-- | 'Error' means errors and failures etc.
pattern Error :: String -> T.Result a
pattern Error e = Result (Left e)

-- | 'Success' means successes and OKs etc.
pattern Success :: a -> T.Result a
pattern Success a = Result (Right a)

{-# COMPLETE Error, Success #-}

-- | Case analysis for the 'Result' type.
--
-- ==== __Examples__
--
-- >>> let s = Success 0
-- >>> let e = Error "critical"
-- >>> result ("Bad: " ++) (("OK: " ++) . show) s
-- "OK: 0"
-- >>> result ("Bad: " ++) (("OK: " ++) . show) e
-- "Bad: critical"
result :: (String -> b) -> (a -> b) -> T.Result a -> b
result f _ (Error e)   = f e
result _ g (Success a) = g a
{-# INLINE result #-}

-- | Convert @'Either' 'String' a@ to @'Result' a@.
fromEither :: Either String a -> T.Result a
fromEither = Result
{-# INLINE fromEither #-}

-- | Convert @'Result' a@ to @'Either' 'String' a@.
toEither :: T.Result a -> Either String a
toEither = runResult
{-# INLINE toEither #-}

-- | Convert @'Result' a@ to @a@ with a default value.
fromSuccess :: a -> T.Result a -> a
fromSuccess _ (Success a) = a
fromSuccess a _           = a
{-# INLINE fromSuccess #-}

-- | Convert @'Result' a@ to @'MonadFail' m => m a@.
toMonadFail :: MonadFail m => T.Result a -> m a
toMonadFail (Success a) = pure a
toMonadFail (Error e)   = fail e
{-# INLINE toMonadFail #-}

-- | Construct and destruct 'T.Result'.
pattern ResultT :: Functor m => m (T.Result a) -> T.ResultT m a
pattern ResultT m <- ((Result <$>) . runExceptT . T.runResultT -> m)
  where ResultT m = T.ResultT $ ExceptT $ runResult <$> m

{-# COMPLETE ResultT #-}

-- | Unwrap 'ResultT'.
runResultT :: Functor m => T.ResultT m a -> m (T.Result a)
runResultT (ResultT m) = m
{-# INLINE runResultT #-}

-- | Map the unwrapped computation using the given function.
mapResultT :: (Functor m, Functor n) => (m (T.Result a) -> n (T.Result b)) -> T.ResultT m a -> T.ResultT n b
mapResultT f = T.mapResultT $ T.runResultT . ResultT . f . runResultT . T.ResultT
{-# INLINE mapResultT #-}
