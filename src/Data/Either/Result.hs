{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE PatternSynonyms            #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- | @'Result' a@ is a wrapper of @'Either' 'String' a@, but 'Result' is an instance of 'MonadFail'.
-- A discussion about 'MonadFail' of 'Either' is <https://gitlab.haskell.org/ghc/ghc/-/issues/12160>.
module Data.Either.Result
  ( type Result
  , pattern Result
  , pattern Error
  , pattern Success
  , runResult
  , result
  , fromEither
  , toEither
  , fromSuccess
  , toMonadFail
  ) where

import Control.Monad.Trans.Result (pattern Error, pattern Result, type Result, pattern Success, fromEither,
                                   fromSuccess, result, runResult, toEither, toMonadFail)
