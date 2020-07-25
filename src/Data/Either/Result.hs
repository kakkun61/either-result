{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- | @'Result' a@ is a wrapper of @'Either' 'String' a@, but 'Result' is an instance of 'MonadFail'.
-- A discussion about 'MonadFail' of 'Either' is <https://gitlab.haskell.org/ghc/ghc/-/issues/12160>.
module Data.Either.Result
  ( type Result
  , pattern Error
  , pattern Success
  , result
  , fromEither
  , toEither
  , fromSuccess
  ) where

import           Prelude             hiding (either)

import           Control.Applicative (Alternative (empty, (<|>)))
import           Control.Monad       (MonadPlus (mplus, mzero))
import           GHC.Generics        (Generic)
import qualified GHC.Show            as S
import           Text.Read           (Read (readPrec))
import qualified Text.Read           as R
import qualified Text.Read.Lex       as R

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail  (MonadFail (fail))
#endif

-- | @'Result' a@ is a wrapper of @'Either' 'String' a@.
newtype Result a =
  Result { either :: Either String a }
  deriving stock (Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Applicative, Monad)

instance Show a => Show (Result a) where
  showsPrec d (Error e) = showParen (S.appPrec < d) $ showString "Error " . showsPrec S.appPrec1 e
  showsPrec d (Success a) = showParen (S.appPrec < d) $ showString "Success " . showsPrec S.appPrec1 a

instance Read a => Read (Result a) where
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

instance Monoid (Result a) where
  mempty = Error "mempty"
  {-# INLINE mempty #-}

  mappend = (<>)
  {-# INLINE mappend #-}

instance Alternative Result where
  empty = Error "empty"
  {-# INLINE empty #-}

  a@(Success _) <|> _ = a
  _ <|> b = b
  {-# INLINE (<|>) #-}

instance MonadFail Result where
  fail = Error
  {-# INLINE fail #-}

instance MonadPlus Result where
  mzero = Error "mzero"
  {-# INLINE mzero #-}

  mplus = (<|>)
  {-# INLINE mplus #-}

-- | 'Error' means errors and failures etc.
pattern Error :: String -> Result a
pattern Error e = Result (Left e)

-- | 'Success' means successes and OKs etc.
pattern Success :: a -> Result a
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
result :: (String -> b) -> (a -> b) -> Result a -> b
result f _ (Error e)   = f e
result _ g (Success a) = g a

-- | Convert @'Either' 'String' a@ to @'Result' a@.
fromEither :: Either String a -> Result a
fromEither = Result
{-# INLINE fromEither #-}

-- | Convert @'Either' 'String' a@ from @'Result' a@.
toEither :: Result a -> Either String a
toEither = either
{-# INLINE toEither #-}

-- | Convert @'Result' a@ to @a@ with a default value.
fromSuccess :: a -> Result a -> a
fromSuccess _ (Success a) = a
fromSuccess a _           = a
