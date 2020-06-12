module HDAssets

where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Control.Promise (Promise, toAffE)
import Foreign (Foreign, unsafeFromForeign)
import Data.Maybe (Maybe)

foreign import data MaybeUndefined :: Type -> Type

foreign import maybeUndefined :: forall a b. b -> (a -> b) -> MaybeUndefined a ->b

foreign import loadAsset_
  :: String
  -> String
  -> Effect (Promise Foreign)

loadAsset :: String -> String -> Aff Foreign
loadAsset name file = toAffE $ loadAsset_ name file

foreign import data WrappedMap :: Type -> Type

foreign import wrLookup :: forall a. String -> WrappedMap a -> Maybe a

foreign import wrKeys :: forall a. WrappedMap a -> Array String

foreign import emptyWrappedMap :: forall a. WrappedMap a


loadFoo :: Aff (Array Int)
loadFoo = do
  -- maybe "try" here?
  f <- toAffE (loadAsset_ "foo" "foo.js")
  pure (unsafeFromForeign f)


loadBar :: Aff (WrappedMap Int)
loadBar = do
  f <- toAffE (loadAsset_ "bar" "foo.js")
  pure (unsafeFromForeign f)
