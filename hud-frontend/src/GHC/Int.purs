
module GHC.Int (
    Int64(..)
) where

import Prelude

import Data.Generic (class Generic)

newtype Int64 = Int64 Int

derive instance eqInt64 :: Eq Int64
derive instance genericInt64 :: Generic Int64
