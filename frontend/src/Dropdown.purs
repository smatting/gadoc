module Dropdown where

import Prelude

import Data.Array as Arr
import Data.Array ((!!), mapWithIndex, length)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks (useLifecycleEffect, useState)
import Halogen.Hooks as Hooks
import Halogen.HTML.Properties as HP
import Halogen.Hooks.Extra.Hooks (useEvent)
import Select (SelectReturn(..), SelectEvent(..), selectInput, useSelect)
import Select as S
import Data.String (contains, Pattern(..), toLower)

type Slot = H.Slot (Const Void) Message

data Message
  = SelectionChanged (Maybe String) (Maybe String)
  | VisibilityChanged Boolean


-- it is unnecessary to export your own input type, but doing so helps if you
-- would like to set some sensible defaults behind the scenes.
type Input =
  { items :: Array String
  , buttonLabel :: String
  }

component :: H.Component HH.HTML (Const Void) Input Message Aff
component = Hooks.component \tokens { items, buttonLabel } -> Hooks.do

  selectEvent <- useEvent
  selection /\ selectionId <- useState Nothing
  allItems /\ allItemsId <- useState ([] :: Array String)
  available /\ availableId <- useState ([] :: Array String)
  overrideButtonLabel /\ overrideButtonLabelId <- useState (buttonLabel :: String)
  override /\ overrideId <- useState (true :: Boolean)

  _ <- useMyEffect {items} allItemsId availableId

  SelectReturn select <- useSelect $ selectInput
    { inputType = S.Text
    {-- , search = Just "base" --}
    , getItemCount = pure (length available)
    , pushSelectedIdxChanged = SelectedIndex >>> selectEvent.push
    , pushNewSearch = NewSearch >>> selectEvent.push
    , pushVisibilityChanged = VisibilityChangedTo  >>> selectEvent.push
    }

  useLifecycleEffect do
    {-- Hooks.put availableId items --}

    void $ selectEvent.setCallback $ Just \_ val -> case val of

      SelectedIndex ix -> do
        oldSelection <- Hooks.get selectionId
        available <- Hooks.get availableId
        let newSelection = available !! ix
        Hooks.put selectionId newSelection
        Hooks.put overrideId false
        Hooks.raise tokens.outputToken $ SelectionChanged oldSelection newSelection
        select.setVisibility S.Off

      NewSearch str -> do
        {-- liftEffect $ log ("new search: " <> str) --}
        items <- Hooks.get allItemsId
        let available' = Arr.filter (\item -> contains (Pattern (toLower str)) (toLower item)) items
        Hooks.put availableId available'

      VisibilityChangedTo vis -> do
        Hooks.raise tokens.outputToken $ VisibilityChanged (vis == S.On)
        case vis of
          S.On -> do
            {-- pure unit --}
            {-- Hooks.put availableId items --}

            {-- liftEffect $ log "became visible" --}
            select.setFocus true

          S.Off -> do
            pure unit
            select.clearSearch
            allItems <- Hooks.get allItemsId
            Hooks.put availableId allItems
            {-- liftEffect $ log "search cleared" --}
            {-- pure unit --}
            {-- select.clearSearch --}
            {-- select.setFocus true --}

    pure Nothing

  Hooks.captures {buttonLabel} Hooks.useTickEffect $ do
    Hooks.put overrideButtonLabelId buttonLabel
    Hooks.put overrideId true
    pure Nothing

  Hooks.pure $
    HH.div
      [ class_ "Dropdown" ]
      [ renderToggle select buttonLabel override overrideButtonLabel selection
      , renderInput select
      , renderContainer select available
      ]
  where

    useMyEffect deps@{items} allItemsId availableId =  Hooks.captures deps Hooks.useTickEffect do
      {-- liftEffect $ log ("items in useTicketEffect: " <> show items) --}
      Hooks.put allItemsId items
      Hooks.put availableId items
      pure Nothing

    renderToggle select buttonLabel override overrideButtonLabel selection =
      whenElem (select.visibility == S.Off) \_ ->
        HH.span
          ( select.setToggleProps [ class_ "package-name" ] )
          [ HH.span_ [ HH.i [ classes_ ["fas", "fa-cube"] ] [] ],
            HH.text (" " <> if override then overrideButtonLabel else fromMaybe "(?)" selection)
          ]

    renderInput select =
      whenElem (select.visibility == S.On) \_ ->
        HH.div_ [
          HH.span
            ( select.setToggleProps [ class_ "package-name" ] )
            [ HH.span_ [ HH.i [ classes_ ["fas", "fa-cube"] ] [] ]
            ],

          HH.p
            [ classes_ ["control", "has-icons-left"] ]
            [ HH.input (select.setInputProps [ HP.value select.search, classes_ ["input", "is-small"] ]),
              HH.span
                [ classes_ ["icon", "is-left"] ]
                [ HH.i [ classes_ ["fas", "fa-filter"] ] []
                ]
            ]
        ]

    renderContainer select items =
      whenElem (select.visibility == S.On) \_ ->
        HH.div
          ( select.setContainerProps [ class_ "Dropdown__container" ] )
          (
            if length items > 0
              then ( mapWithIndex (renderItem select) items )
              else [HH.div_ [HH.text $ "No packages match \"" <> select.search <> "\""]]
          )

    renderItem select index item =
      HH.div
        ( select.setItemProps index
            [ classes_
                [ "Dropdown__item"
                , "noselect"
                , "Dropdown__item--highlighted"
                    # guard (select.highlightedIndex == Just index)
                ]
            ]
        )
        [ HH.text item ]


class_ :: forall p i. String -> HH.IProp (class :: String | i) p
class_ = HP.class_ <<< HH.ClassName

classes_ :: forall p i. Array String -> HH.IProp (class :: String | i) p
classes_ = HP.classes <<< map HH.ClassName

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML i p) -> HH.HTML i p
whenElem cond render = if cond then render unit else HH.text ""
