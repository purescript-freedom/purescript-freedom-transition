module Main where

import Prelude

import Data.Array ((..), snoc, last, delete)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Freedom as Freedom
import Freedom.Markup as H
import Freedom.TransformF.Simple (VQueryF, transformF, select, reduce)
import Freedom.Transition (transitionGroup)
import Freedom.VNode (VNode)

type State = Array Int

type Html = VNode VQueryF State

main :: Effect Unit
main = Freedom.run
  { selector: "#app"
  , initialState
  , subscriptions: []
  , transformF
  , view
  }

-- State

initialState :: State
initialState = 0 .. 2

-- View

view :: State -> Html
view state =
  H.el $ H.div # H.kids
    [ H.el $ H.h1 # H.kids [ H.t "Transition demo" ]
    , H.el $ H.button
        # H.onClick (const addItem)
        # H.kids [ H.t "Add Item" ]
    , transitionGroup transition $ item <$> state
    ]
  where
    transition =
      { enter: Just
          { startClassName: "enter"
          , activeClassName: "enter-active"
          , delay: Milliseconds 300.0
          }
      , leave: Just
          { startClassName: "leave"
          , activeClassName: "leave-active"
          , delay: Milliseconds 300.0
          }
      }
    addItem = do
      lastInt <- fromMaybe (-1) <<< last <$> select
      reduce \s -> snoc s $ lastInt + 1

item :: Int -> Html
item i =
  H.keyed (show i) $ H.el $ H.div
    # H.css css
    # H.onClick (const removeItem)
    # H.kids [ H.t $ "Item " <> show i <> ": if clicked, will delete"]
  where
    removeItem = reduce $ delete i
    css =
      """
      .& {
        display: flex;
        justify-content: flex-start;
        align-items: center;
        padding: 8px 16px;
        background: #EEE;
        cursor: pointer;
      }
      .&:hover {
        background: #DDD;
      }
      .&.enter {
        opacity: 0;
      }
      .&.enter.enter-active {
        opacity: 1;
        transition: opacity 0.3s linear;
        pointer-events: none;
      }
      .&.leave {
        opacity: 1;
      }
      .&.leave.leave-active {
        opacity: 0;
        transition: opacity 0.3s linear;
        pointer-events: none;
      }
      """
