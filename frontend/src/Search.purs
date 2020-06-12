module Search
where

import Prelude
import Data.Maybe
import Effect
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Console (log)
import Index (CompressedTarget(..), Target(..), loadDocState, loadKey2Target, loadKeys, DocState(..), uncompress, Url(..))
import Query (Query(..))
import Data.String.CodeUnits (uncons, singleton)
import Data.Char.Unicode (toLower, isUpper)
import Data.String.Common (toLower) as SC
import Data.Array as Arr
import Data.List.Lazy as LL
import HDAssets
import Debug.Trace (trace)
import Effect.Class (liftEffect)
import Data.Map as Map
import Data.Tuple
import Data.Tuple.Nested
import Data.List (List)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Partial.Unsafe (unsafePartial)
import Data.Foldable (all)

foreign import textSearch :: Array String -> Array String -> Boolean -> Array String

normalizeString :: String -> String
normalizeString s =
  case uncons s of
      Nothing -> ""
      Just uc ->
        (if isUpper uc.head
           then " " <> singleton (toLower uc.head)
           else singleton uc.head
        ) <> SC.toLower uc.tail


keywords :: Array Query -> Array String
keywords qs = Arr.catMaybes $ map f qs
  where
    f (QueryName {fromQueryName}) = Just (normalizeString fromQueryName)
    f _ = Nothing


packageScopes :: Array Query -> Array (Tuple Boolean String)
packageScopes qs = Arr.catMaybes $ map f qs
  where
    f (QueryScope {scopeCategory, scopeInclude, scopeValue}) =
        if scopeCategory == "package"
          then Just (scopeInclude /\ scopeValue)
          else Nothing
    f _ = Nothing


packageFilterScopes :: LL.List Target -> Array (Tuple Boolean String) -> LL.List Target
packageFilterScopes targets scopes =
  if Arr.length scopes == 0
    then targets
    else LL.filter (\t ->  all (\scope -> inScope scope t) scopes) targets
  where
    inScope (include /\ p) t =
      case (t.package) of
        Just (tp /\ _) -> p == tp
        Nothing -> false


lookupTargets :: DocState -> WrappedMap (Array CompressedTarget) -> Array String -> LL.List Target
lookupTargets docState lookup keys = do
  key <- LL.fromFoldable keys
  ct <- LL.fromFoldable (fromMaybe [] (wrLookup key lookup))
  pure (uncompress docState ct)


search :: Array Query -> Aff (Array Target)
search qs = do
  lookup <- loadKey2Target
  docState <- loadDocState
  haystack <- loadKeys
  let needles = keywords qs
  if Arr.length needles == 0
    then pure []
    else do
      let keysFound = Arr.take 400 $ textSearch haystack needles false
      let targets = lookupTargets docState lookup keysFound
      let targetsFiltered = packageFilterScopes targets (packageScopes qs)
      pure $ Arr.fromFoldable targetsFiltered


toNonEmptyPartial :: forall a. Array a -> NonEmptyArray a
toNonEmptyPartial xs = unsafePartial $ fromJust $ (NE.fromArray xs)


-- Group duplicated targets (e.g. re-exports) together.
deDup:: Array Target -> Array (NonEmptyArray Target)
deDup tgts = Arr.fromFoldable $ (toNonEmptyPartial <<< Arr.fromFoldable) <$> sortedTargets
  where
    sortedTargets = Map.values (Map.fromFoldable (Map.values tgtMap2))

    tgtMap2 = Map.fromFoldableWith
                (\(Tuple n ts) (Tuple n' ts') -> Tuple (min n n') (ts <> ts'))
                tgtMap1

    tgtMap1 = LL.zipWith
                (\n t -> Tuple (simple t) (Tuple n (LL.singleton t)))
                (LL.range 0 (Arr.length tgts - 1))
                (LL.fromFoldable tgts)

    -- TODO: Include also docs (or hash thereof) and types to prevent
    -- grouping of items with same signature but different semantics
    simple :: Target -> String
    simple t = t.item

type MaybePackage = Maybe (Tuple String Url)

-- subdivide a group of re-exports by their packages
groupByModule
  :: NonEmptyArray Target
  -> Tuple Target (Array (NonEmptyArray Target))
groupByModule tgts =
  let tgts' :: NonEmptyArray Target
      tgts' = NE.sortWith (\x -> (x.module_ <#> (\(Tuple m _) -> (Tuple (SC.toLower m.package) m.module)))) tgts

      g :: Array (NonEmptyArray Target)
      g = Arr.groupBy (\a b -> a.package == b.package) (NE.toArray tgts')

  in Tuple (NE.head tgts') g


packagesInResult :: Array Target -> Array String
packagesInResult targets =
  Arr.nub $
    Arr.catMaybes
      $ Arr.fromFoldable (targets <#> \t -> (t.package <#> \(p /\ _) -> p))


search2 :: Array Query -> Aff (Tuple (Array String) (Array (Tuple Target (Array (NonEmptyArray Target)))))
search2 qs = do
  tgts <- search qs
  let packages = packagesInResult tgts
  let grouped = deDup tgts <#> groupByModule
  pure $ Tuple packages grouped
