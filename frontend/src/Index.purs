module Index

where

import Prelude

import HDAssets
import Effect.Aff (Aff)
import Data.Either
import Control.Promise (Promise, toAffE)
import Foreign (Foreign, unsafeFromForeign)
import Data.Array as Arr
import Data.String (indexOf, Pattern(..), contains, lastIndexOf, length)
import Data.Tuple (Tuple(..))
import Data.Maybe
import Data.Show
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Partial.Unsafe (unsafePartial)


foreign import unEscapeHTML :: String -> String

regexPartial :: String -> Regex.Regex
regexPartial s = unsafePartial $ fromRight $ Regex.regex s global

-- TODO: prevent from remove <$>, <#> etc.
tagRegex :: Regex.Regex
tagRegex = regexPartial "<[^>]+>"

stripTags :: String -> String
stripTags s =
  Regex.replace tagRegex "" s

type RefUrl = { i :: Int, f :: MaybeUndefined String }

type CompressedTarget =
  { u :: RefUrl,
    t :: Int,
    i :: String,
    p :: MaybeUndefined {r :: Int, u :: RefUrl},
    m :: MaybeUndefined {r :: Int, u :: RefUrl}
  }

loadKey2Target :: Aff (WrappedMap (Array CompressedTarget))
loadKey2Target = do
  f <- toAffE (loadAsset_ "targets" "targets.js")
  pure (unsafeFromForeign f)

loadKeys :: Aff (Array String)
loadKeys = Arr.fromFoldable <<< wrKeys <$> loadKey2Target

type DocState =
  {
    urls :: Array String,
    packages :: Array String,
    modules :: Array {package:: String, module:: String},
    docTitle :: String
  }

loadDocState :: Aff DocState
loadDocState = do
  f <- toAffE (loadAsset_ "docstate" "docstate.js")
  pure (unsafeFromForeign f)

type Url = String

data TargetType = PackageType | ModuleType | NoneType

instance ttshow :: Show TargetType where
    show PackageType = "PackageType"
    show ModuleType = "ModuleType"
    show NoneType = "NoneType"

type Target =
  {
    url :: Url,
    type_ :: TargetType,
    item :: String,
    package :: Maybe (Tuple String Url),
    module_ :: Maybe (Tuple {package:: String, module:: String} Url)
  }

endsWith :: String -> String -> Boolean
endsWith haystack needle =
  let k = length needle
      n = length haystack
  in
    case lastIndexOf (Pattern needle) haystack of
      Nothing -> false
      Just i -> i + k == n


mkUrl :: DocState -> RefUrl -> Url
mkUrl ds refUrl =
  let url = maybe
              "#"
              (\baseurl -> maybeUndefined baseurl (\f -> baseurl <> f) refUrl.f )
              (Arr.index ds.urls refUrl.i)
  in if url `endsWith` "/"
       then url <> "index.html"
       else url


mkTargetType :: Int -> TargetType
mkTargetType x =
  case x of
    0 -> NoneType
    1 -> ModuleType
    _ -> PackageType


mkPackage :: DocState -> Int -> String
mkPackage ds i = fromMaybe "<unknown package>" (Arr.index ds.packages i)


type Module = {package:: String, module :: String}

mkModule :: DocState -> Int -> Module
mkModule ds i = fromMaybe {package: "<unknown package>", module: "<unknown module>"} (Arr.index ds.modules i)


uncompress :: DocState -> CompressedTarget -> Target
uncompress ds ct =
  {
    url: mkUrl ds ct.u,
    type_: mkTargetType ct.t,
    item: stripTags (unEscapeHTML ct.i),
    package:
      maybeUndefined
      Nothing
      (\p ->
        Just $
          Tuple
            (mkPackage ds p.r)
            (mkUrl ds p.u)
      )
      ct.p,
    module_:
      maybeUndefined
      Nothing
      (\m ->
        Just $
          Tuple
            (mkModule ds m.r)
            (mkUrl ds m.u)
      )
      ct.m
  }


-- Search ------------------------------------------------------------------------

queryToToken :: String -> String
queryToToken s = s

search :: String -> Array String -> Array String
search q arr =
  let qs = queryToToken q
  in Arr.filter (\s -> (contains (Pattern qs) s)) arr
