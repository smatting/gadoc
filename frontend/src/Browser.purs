module Browser where

import Prelude

import Data.Maybe (Maybe(..), maybe, fromMaybe, fromJust)
import Data.Tuple
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Effect.Console (log)
import Effect.Class (liftEffect)
import HDAssets
import Effect.Aff (Aff)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

import Data.Array as Arr
import Data.String (contains, Pattern(..), split, toLower, length, stripPrefix)
import Index (Target(..), loadKey2Target, loadDocState, loadKeys, TargetType(..))
import Search (textSearch, search, search2)
import Query (lexer, scope, parseQuery, renderQuery, Query(..))

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE

import Web.Event.Event (currentTarget, target, preventDefault)
import Web.UIEvent.MouseEvent (toEvent)
import Web.DOM.Element (fromEventTarget, getAttribute, Element(..))
import Web.HTML.HTMLIFrameElement (HTMLIFrameElement, fromElement, contentWindow)
import Web.HTML.Window (location)
import Web.HTML.Location (href)

import Data.Tuple.Nested ((/\))
import Halogen.Hooks as Hooks
import Effect.Aff.Class

import Dropdown as Dropdown
import Dropdown (Message(..))
import Data.Symbol (SProxy(..))

import PackageDocs (PackageDocs, loadAllPackageDocs)


m2l :: forall a. Maybe a -> Array a
m2l Nothing = []
m2l (Just x) = [ x ]


{-- type ChildSlots = --}
{--   ( dropdown :: Dropdown.Slot Unit --}
{--   ) --}

_dropdown :: SProxy "dropdown"
_dropdown = SProxy


data MainWindow = WindowSearchResults | WindowIFrame


component :: forall query i o. H.Component HH.HTML query i o Aff
component = Hooks.component \_ _ -> Hooks.do
  queryInput /\ queryInputId <- Hooks.useState ""
  query /\ queryId <- Hooks.useState ([] :: Array Query)
  queryOnlyCurrentPackage /\ queryOnlyCurrentPackageId <- Hooks.useState false
  results /\ resultsId <- Hooks.useState ( [] :: Array (Tuple Target (Array (NonEmptyArray Target))))
  packagesInResults /\ packagesInResultsId <- Hooks.useState ( [] :: Array String )
  iframeSrc /\ iframeSrcId <- Hooks.useState "about:blank"
  currentPackage /\ currentPackageId <- Hooks.useState (Nothing :: Maybe String)
  packageDocsVisibile /\ packageDocsVisibileId  <- Hooks.useState true
  currentPackageDocs /\ currentPackageDocsId <- Hooks.useState (Nothing :: Maybe PackageDocs)
  allPackageDocs /\ allPackageDocsId <- Hooks.useState (emptyWrappedMap :: WrappedMap PackageDocs)
  moduleFilterInput /\ moduleFilterInputId <- Hooks.useState ("" :: String)
  mainWindow /\ mainWindowId <- Hooks.useState WindowSearchResults
  loadingAssets /\ loadingAssetsId <- Hooks.useState true

  let setIFrame url = do
        Hooks.put iframeSrcId url
        Hooks.put mainWindowId WindowIFrame

  let setCurrentPackage = \allPackageDocs p -> (
      do
        Hooks.put queryOnlyCurrentPackageId false
        case wrLookup p allPackageDocs of
           Nothing -> pure unit
           Just packageDocs -> do
             Hooks.put currentPackageId (Just p)
             Hooks.put currentPackageDocsId (Just packageDocs)
             Hooks.put moduleFilterInputId ""
             setIFrame (packageDocs.url <> "/index.html")
  )

  let onClickIFrameUrl url = HE.onClick (\e -> Just do
        liftEffect $ preventDefault (toEvent e)
        setIFrame url
      )

  let runSearch = do
         liftEffect $ log $ "this is runSearch"
         qocp <- Hooks.get queryOnlyCurrentPackageId
         qi <- Hooks.get queryInputId

         Hooks.put mainWindowId WindowSearchResults
         let q' = parseQuery qi
         let q = if qocp
           then
            case currentPackage of
              Just p -> Arr.snoc q' (QueryScope {scopeInclude: true, scopeCategory: "package", scopeValue: p})
              _ -> q'
           else q'

         liftEffect $ log $ "runSearch qith query " <> show q

         Hooks.put queryId q
         ps /\ results <- liftAff (search2 q)
         Hooks.put resultsId results
         Hooks.put packagesInResultsId ps


  Hooks.useLifecycleEffect do
    liftEffect $ log "loading"
    allPackageDocs' <- liftAff loadAllPackageDocs

    _ <- liftAff loadKey2Target
    _ <- liftAff loadDocState
    _ <- liftAff loadKeys

    liftEffect $ log "loaded"
    {-- liftEffect $ log (show (wrKeys allPackageDocs')) --}
    Hooks.put allPackageDocsId allPackageDocs'
    setCurrentPackage allPackageDocs' "base"
    Hooks.put loadingAssetsId false
    pure Nothing


  let inputBox =
        HH.input
          [ HP.type_ HP.InputText,
            HP.value queryInput,
            HE.onValueInput \s -> Just $ Hooks.put queryInputId s,
            HP.disabled loadingAssets,
            HE.onKeyDown
              (\event ->
                case KeyboardEvent.code event of
                  "Enter" -> Just runSearch
                  _ -> Nothing
              ),
            class_ "input"
          ]

      packageDropDown =
        HH.slot
          _dropdown
          unit
          Dropdown.component
          ({items: wrKeys allPackageDocs, buttonLabel: fromMaybe "" currentPackage })
          (\msg -> case msg of
            SelectionChanged _ selectedPackage ->
                case selectedPackage of
                  Nothing -> Nothing
                  Just p -> Just $ do
                    allPackageDocs <- Hooks.get allPackageDocsId
                    setCurrentPackage allPackageDocs p
            VisibilityChanged visible -> Just $
              Hooks.put packageDocsVisibileId (not visible)
          )

      resultList =
        HH.ol [ HP.class_ (HH.ClassName "result-list") ]  (
          Arr.take 20 results <#> (\(Tuple target tPackages) ->
            HH.li_  [
              HH.a
                [ HP.href target.url,
                  HP.class_ (HH.ClassName "src"),
                  HE.onClick (\e -> do
                    case packageAndUrl target of
                      Nothing -> Just $ liftEffect $ preventDefault (toEvent e)
                      Just (p /\ url) -> Just do
                        liftEffect $ log ("p " <> p)

                        liftEffect $ preventDefault (toEvent e)
                        setCurrentPackage allPackageDocs p
                        setIFrame target.url
                    )
                ]
                [ HH.text target.item],

              HH.ol [ HP.class_ (HH.ClassName "result-package-list") ] (
                tPackages <#> (\moduleTargets ->
                  let
                    t = NE.head moduleTargets
                  in
                    HH.li_ (
                      (m2l (t.package <#> \(Tuple name url) ->
                        HH.a
                          [ HP.href url,
                            HP.class_ (HH.ClassName "result-detail-package"),
                            onClickIFrameUrl url
                          ]
                          [ HH.text name ]))
                      <>
                      [
                        HH.ol [ HP.class_ (HH.ClassName "result-module-list") ] (
                          NE.toArray $
                            moduleTargets <#> (\tt ->
                              HH.li_ ( m2l (
                                tt.module_ <#> (\(m /\ _) ->
                                  HH.a
                                    [ HP.href tt.url,
                                      HP.class_ (HH.ClassName "result-detail-module"),
                                      onClickIFrameUrl tt.url
                                    ]
                                    [ HH.text m.module ])
                              ))
                            )
                        )
                      ]
                    )
                )
              )
            ]
          ))

  Hooks.pure do
    HH.div_ [
      HH.div
        [ HP.id_ "overlay" ]
        [
          {-- HH.div --}
          {--   [ classes_ ["collapse-level-item", "level-item"] ] --}
          {--   [ HH.button --}
          {--       [ classes_ ["button", "is-light"] ] --}
          {--       [ HH.i [ classes_ [ "fa", "fa-angle-double-left" ] ] [] ] --}
          {--   ], --}

          HH.div
            [ classes_ ["field", "has-addons"] ]
            [
              HH.div
                [ classes_ [ "control", "has-icons-left" ]
                ]
                [ inputBox,
                  HH.span
                    [ classes_ [ "icon", "is-left" ] ]
                    [ HH.i [ classes_ ["fas", "fa-search"] ] [] ]
                ]
              ],

          HH.div
            [ class_ "field" ]
            [ HH.div
                [ class_ "control" ]
                [ HH.label
                    [ classes_ ["checkbox", "noselect"]
                    ]
                    [ HH.input [
                        HP.type_ HP.InputCheckbox,
                        HP.checked queryOnlyCurrentPackage,
                        HE.onChecked (\checked -> Just do
                          Hooks.put queryOnlyCurrentPackageId checked
                          liftEffect $ log ("checked: " <> show checked)
                          when (length queryInput > 0) runSearch
                        )
                    ],
                      HH.text (" search only in "),
                      HH.b_ [ HH.text (fromMaybe "" currentPackage) ]
                    ]
                ]
            ],

          HH.hr_,

          packageDropDown,

          whenElem packageDocsVisibile \_ ->
            HH.div
              [ classes_ ["module-search-level-item", "level-item"] ]
              [ HH.div
                  [ classes_ ["field", "has-addons"] ]
                  [ HH.p
                      [ classes_ ["control", "has-icons-left"] ]
                      [ HH.input
                          [ classes_ [ "input", "module-list-search", "is-small" ],
                            HP.type_ HP.InputText,
                            HP.value moduleFilterInput,
                            HE.onValueInput \s -> Just $ Hooks.put moduleFilterInputId s
                          ],
                        HH.span
                          [ classes_ ["icon", "is-left"] ]
                          [ HH.i [ classes_ ["fas", "fa-filter"] ] []
                          ]
                      ],
                    HH.p
                      [ class_ "control" ]
                      [ HH.button
                          [ classes_ ["button", "is-small"],
                            HP.disabled (length moduleFilterInput == 0),
                            HE.onClick \_ -> Just $ Hooks.put moduleFilterInputId ""
                            ]
                          [ HH.span
                              [ class_ "icon" ]
                              [ HH.i [ classes_ ["fas", "fa-eraser" ] ] [] ]
                          ]
                      ]
                  ]
              ],

          whenElem packageDocsVisibile \_ ->
            renderJust currentPackageDocs \pdocs -> (
              HH.ul
                [ class_ "module-list" ]
                ( [ HH.li_
                    [ HH.a
                      [ HP.href pdocs.url,
                        class_ "package-contents",
                        onClickIFrameUrl (pdocs.url <> "index.html")
                        ]
                      [ HH.text "Package Contents" ]
                    ]
                  ] <>
                  let
                    q = toLower moduleFilterInput
                    filteredModules =
                      if length q > 0
                        then Arr.filter (\mod -> contains (Pattern q) (toLower mod.name)) pdocs.modules
                        else pdocs.modules
                  in
                    ( filteredModules <#> \mod ->
                        HH.li_
                          [ HH.a
                              [ HP.href mod.url,
                                onClickIFrameUrl mod.url
                              ]

                              [ HH.text mod.name ]
                          ]
                    )
                )
            )

        ],

      case mainWindow of
        WindowSearchResults ->
          HH.div
            [ classes_ ["main-content"] ]

            [ HH.div
                [ class_ "rendered-query" ]
                [ HH.text (renderQuery query) ]

            , whenElem (Arr.length packagesInResults > 1) \_ ->
                ( HH.ol [class_ "search-refinement"]  (Arr.take 10 packagesInResults <#> \p ->
                    HH.li_ [
                      HH.a
                        [ HP.href "#",
                          HE.onClick \e -> Just do
                            void $ Hooks.modify queryInputId (\q -> q <> " +" <> p)
                            runSearch
                        ]
                        [ HH.text ("+" <> p) ]
                    ]
                  )
                )

            , resultList
            ]

        WindowIFrame ->
          HH.iframe [ HP.name "doc",
                      classes_ ["main-iframe"],
                      HP.src iframeSrc
                      ]
   ]


stripPrefix' :: String -> String -> String
stripPrefix' q s = fromMaybe s (stripPrefix (Pattern q) s)

type Url = String

packageAndUrl :: Target -> Maybe (Tuple String Url)
packageAndUrl target =
  case target.type_ of
    PackageType -> Just $ stripPrefix' "package " target.item /\ target.url
    _ -> target.package <#> \(p /\ _) -> p /\ target.url

class_ :: forall p i. String -> HH.IProp (class :: String | i) p
class_ = HP.class_ <<< HH.ClassName

classes_ :: forall p i. Array String -> HH.IProp (class :: String | i) p
classes_ = HP.classes <<< map HH.ClassName

renderJust :: forall p i a. Maybe a -> (a -> HH.HTML i p) -> HH.HTML i p
renderJust x render = maybe (HH.text "") render x

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML i p) -> HH.HTML i p
whenElem cond render = if cond then render unit else HH.text ""
