module Example
  ( foo,
  )
where

import Prelude

{- | Accepts a unit, and produces a unit.

This function is pointless, in both senses of the word, as:

* It serves no useful purpose.
* Its definition has no point.

-}
foo :: () -> ()
foo = id
