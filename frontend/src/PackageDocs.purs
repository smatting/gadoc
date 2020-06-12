module PackageDocs

where

import Effect.Aff (Aff)
import Prelude
import HDAssets (loadAsset, MaybeUndefined(..), WrappedMap(..))
import Foreign (Foreign, unsafeFromForeign)


-- TODO: check if MaybeUndefined
-- TODO: add package name
type PackageDocs =
  { url :: String,
    modules :: Array ({url :: String, name :: String})
  }


loadAllPackageDocs :: Aff (WrappedMap PackageDocs)
loadAllPackageDocs =
 unsafeFromForeign <$> loadAsset "modules" "modules.js"
