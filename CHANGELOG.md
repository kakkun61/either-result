# Revision history for either-result

## 0.2.0.0

*2020.08.04*

### Breaking changes

- Change `Data.Either.Result.Result` from a `newtype` to a type synonym.
  - Get `Result` data constructor not to be exposed
    - Use `Result` pattern instead
    - Or use `Control.Monad.Trans.Except.Result.ResultT` data constructor instead

### Other changes

- Add monad transformers

## 0.1.2.0

*2020.07.31*

- Add `toMonadFail`.

## 0.1.1.0

*2020.07.28*

- Expose a value constructor and a field for coercion.

## 0.1.0.0

*2020.07.26*

- Release.
