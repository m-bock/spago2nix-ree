module Data.Codec.Argonaut.Extra where

import Prelude
import Data.Codec.Argonaut (JsonCodec, prismaticCodec)
import Data.Codec.Argonaut.Compat as Codec.Compat
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object

codecMap :: forall v. JsonCodec v -> JsonCodec (Map String v)
codecMap codecA =
  prismaticCodec decoder encoder
    $ Codec.Compat.foreignObject codecA
  where
  decoder :: Object v -> Maybe (Map String v)
  decoder obj =
    obj
      # (Object.toUnfoldable :: _ -> Array _)
      # Map.fromFoldable
      # Just

  encoder :: Map String v -> Object v
  encoder xs =
    xs
      # (Map.toUnfoldable :: _ -> Array _)
      # Object.fromFoldable
