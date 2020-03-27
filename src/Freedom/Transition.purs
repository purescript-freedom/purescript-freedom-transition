module Freedom.Transition
  ( Transition
  , Config
  , transitionGroup
  ) where

import Prelude

import Control.Monad.Writer (runWriter, tell)
import Data.Array (any, filter, findIndex, foldRecM, insertAt, length, modifyAtIndices, nubEq, snoc)
import Data.Foldable (elem)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, split)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Timer (setTimeout)
import Foreign.Object (alter, lookup, update)
import Freedom.Markup as H
import Freedom.UI (Operation, VNode, VObject, modifyVObject)
import Freedom.UI.Class (getKey)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Element as E

-- | The type of transition
-- |
-- | - `startClassName`: The class name when start transition
-- | - `activeClassName`: The class name for transition
-- | - `delay`: Delay for real DOM manipulation. you should set transition duration
type Transition =
  { startClassName :: String
  , activeClassName :: String
  , delay :: Milliseconds
  }

-- | The type of transition config
-- |
-- | - `enter`: The transition for adding nodes
-- | - `leave`: The transition for removing nodes
-- |
-- | You have to style children like:
-- |
-- | ```
-- | .enter {
-- |   opacity: 0;
-- | }
-- | .enter.enter-active {
-- |   opacity: 1;
-- |   transition: opacity 0.3s linear;
-- | }
-- | .leave {
-- |   opacity: 1;
-- | }
-- | .leave.leave-active {
-- |   opacity: 0;
-- |   transition: opacity 0.3s linear;
-- | }
-- | ```
type Config =
  { enter :: Maybe Transition
  , leave :: Maybe Transition
  }

-- | Transition passed children.
transitionGroup
  :: forall state
   . Config
  -> Array (VNode state)
  -> VNode state
transitionGroup config children =
  H.div
    # H.renderingManually
    # H.didCreate (renderChildren children)
    # H.didUpdate (transitionChildren config children)
    # H.didDelete deleteChildren

renderChildren
  :: forall state
   . Array (VNode state)
  -> E.Element
  -> Operation state
  -> Effect Unit
renderChildren children element { renderer } =
  renderer.renderChildren (E.toNode element) children

deleteChildren
  :: forall state
   . E.Element
  -> Operation state
  -> Effect Unit
deleteChildren element { renderer } =
  renderer.renderChildren (E.toNode element) []

transitionChildren
  :: forall state
   . Config
  -> Array (VNode state)
  -> E.Element
  -> Operation state
  -> Effect Unit
transitionChildren config currents element { renderer } = do
  let node = E.toNode element
  prevs <- renderer.getLatestRenderedChildren
  Tuple currents_ enterings <- pure $ willEnterVNodes config prevs currents
  Tuple currents' leavings <- pure $ willLeaveVNodes config prevs currents_
  if length enterings <= 0 && length leavings <= 0
    then renderer.renderChildren node currents
    else do
      renderer.renderChildren node currents'
      void $ setTimeout 0 $ renderer.renderChildren node $ startTransitionVNodes config currents'
      when (length enterings > 0) $ flip (maybe $ pure unit) config.enter \t ->
        void $ setTimeout (ceil $ unwrap t.delay) do
          prevs' <- renderer.getLatestRenderedChildren
          renderer.renderChildren node $ removeTransitionClassNames t enterings prevs'
      when (length leavings > 0) $ flip (maybe $ pure unit) config.leave \t ->
        void $ setTimeout (ceil $ unwrap t.delay) do
          prevs' <- renderer.getLatestRenderedChildren
          renderer.renderChildren node $ filter (not <<< isIncluded leavings) prevs'

removeTransitionClassNames
  :: forall state
   . Transition
  -> Array (VNode state)
  -> Array (VNode state)
  -> Array (VNode state)
removeTransitionClassNames { startClassName, activeClassName } enterings vnodes =
  remove <$> vnodes
  where
    remove vnode =
      if isIncluded enterings vnode
        then modifyVObject (removeClassName startClassName >>> removeClassName activeClassName) vnode
        else vnode

startTransitionVNodes
  :: forall state
   . Config
  -> Array (VNode state)
  -> Array (VNode state)
startTransitionVNodes config firstRenderingVNodes =
  addEnterActive <<< addLeaveActive <$> firstRenderingVNodes
  where
    addEnterActive vnode =
      case config.enter of
        Nothing -> vnode
        Just transition -> addActiveClassName transition vnode
    addLeaveActive vnode =
      case config.leave of
        Nothing -> vnode
        Just transition -> addActiveClassName transition vnode

addActiveClassName
  :: forall state
   . Transition
  -> VNode state
  -> VNode state
addActiveClassName { startClassName, activeClassName } =
  modifyVObject \vobject ->
    if hasClassName startClassName vobject && (not $ hasClassName activeClassName vobject)
      then addClassName activeClassName vobject
      else vobject

willEnterVNodes
  :: forall state
   . Config
  -> Array (VNode state)
  -> Array (VNode state)
  -> Tuple (Array (VNode state)) (Array (VNode state))
willEnterVNodes config prevs currents =
  case config.enter of
    Nothing -> Tuple currents []
    Just { startClassName } ->
      let addFirstClassName acc vnode =
            if isIncluded prevs vnode
              then pure acc
              else do
                tell [ vnode ]
                pure $ modifyAtIndices
                  [ unsafePartial $ fromJust $ findIndex (equalKey vnode) acc ]
                  (modifyVObject $ addClassName startClassName)
                  acc
       in runWriter $ foldRecM addFirstClassName currents currents

willLeaveVNodes
  :: forall state
   . Config
  -> Array (VNode state)
  -> Array (VNode state)
  -> Tuple (Array (VNode state)) (Array (VNode state))
willLeaveVNodes config prevs currents =
  case config.leave of
    Nothing -> Tuple currents []
    Just { startClassName } ->
      let addFirstClassName acc vnode =
            if isIncluded currents vnode
              then pure acc
              else do
                tell [ vnode ]
                pure $ unsafePartial $ fromJust $ insertAt
                  (unsafePartial $ fromJust $ findIndex (equalKey vnode) prevs)
                  (modifyVObject (addClassName startClassName) vnode)
                  acc
       in runWriter $ foldRecM addFirstClassName currents prevs

hasClassName :: forall state. String -> VObject state -> Boolean
hasClassName name vobject =
  case lookup "className" vobject.props of
    Nothing -> false
    Just val ->
      elem name $ split (Pattern " ") val

addClassName :: forall state. String -> VObject state -> VObject state
addClassName name vobject =
  vobject { props = alter add' "className" vobject.props }
  where
    add' current =
      case current of
        Nothing -> Just name
        Just val ->
          Just $ joinWith " " $ nubEq $ snoc (split (Pattern " ") val) name

removeClassName :: forall state. String -> VObject state -> VObject state
removeClassName name vobject =
  vobject { props = update remove' "className" vobject.props }
  where
    remove' val =
      let classes = filter (_ /= name) $ split (Pattern " ") val
       in if length classes <= 0 then Nothing else Just $ joinWith " " classes

isIncluded
  :: forall state
   . Array (VNode state)
  -> VNode state
  -> Boolean
isIncluded vnodes vnode =
  any (equalKey vnode) vnodes

equalKey
  :: forall state
   . VNode state
  -> VNode state
  -> Boolean
equalKey a b = getKey 0 a == getKey 0 b
