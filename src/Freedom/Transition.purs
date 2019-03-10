module Freedom.Transition
  ( Transition
  , Config
  , transitionGroup
  ) where

import Prelude

import Control.Monad.Free.Trans (FreeT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (runWriter, tell)
import Data.Array (length, any, snoc, filter, foldRecM, modifyAtIndices, findIndex, insertAt)
import Data.Foldable (elem)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split, joinWith)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Foreign.Object (alter, lookup, update)
import Freedom.Markup as H
import Freedom.Renderer.Diff (key)
import Freedom.VNode (VNode(..), VElement(..), VObject, VRender, operations)
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
  :: forall f state
   . Functor (f state)
  => Config
  -> Array (VNode f state)
  -> VNode f state
transitionGroup config children =
  H.op $ H.div
    # H.didCreate renderChildren
    # H.didUpdate (transitionChildren config)
    # H.didDelete deleteChildren
    # H.kids children

renderChildren
  :: forall f state
   . Functor (f state)
  => E.Element
  -> FreeT (f state) (VRender f state) Unit
renderChildren element = do
  r <- lift operations
  liftEffect $ r.getOriginChildren >>= r.renderChildren (E.toNode element)

deleteChildren
  :: forall f state
   . Functor (f state)
  => E.Element
  -> FreeT (f state) (VRender f state) Unit
deleteChildren element = do
  r <- lift operations
  liftEffect $ r.renderChildren (E.toNode element) []

transitionChildren
  :: forall f state
   . Functor (f state)
  => Config
  -> E.Element
  -> FreeT (f state) (VRender f state) Unit
transitionChildren config element = do
  let node = E.toNode element
  r <- lift operations
  liftEffect do
    currents <- r.getOriginChildren
    prevs <- r.getLatestRenderedChildren
    Tuple currents_ enterings <- pure $ willEnterVNodes config prevs currents
    Tuple currents' leavings <- pure $ willLeaveVNodes config prevs currents_
    if length enterings <= 0 && length leavings <= 0
      then r.renderChildren node currents
      else do
        r.renderChildren node currents'
        void $ setTimeout 0 $ r.renderChildren node $ startTransitionVNodes config currents'
        when (length enterings > 0) $ flip (maybe $ pure unit) config.enter \t ->
          void $ setTimeout (ceil $ unwrap t.delay) do
            prevs' <- r.getLatestRenderedChildren
            r.renderChildren node $ removeTransitionClassNames t enterings prevs'
        when (length leavings > 0) $ flip (maybe $ pure unit) config.leave \t ->
          void $ setTimeout (ceil $ unwrap t.delay) do
            prevs' <- r.getLatestRenderedChildren
            r.renderChildren node $ filter (not <<< isIncluded leavings) prevs'

removeTransitionClassNames
  :: forall f state
   . Transition
  -> Array (VNode f state)
  -> Array (VNode f state)
  -> Array (VNode f state)
removeTransitionClassNames { startClassName, activeClassName } enterings vnodes =
  remove <$> vnodes
  where
    remove vnode =
      if isIncluded enterings vnode
        then removeClassName startClassName >>> removeClassName activeClassName $ vnode
        else vnode

startTransitionVNodes
  :: forall f state
   . Config
  -> Array (VNode f state)
  -> Array (VNode f state)
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
  :: forall f state
   . Transition
  -> VNode f state
  -> VNode f state
addActiveClassName { startClassName, activeClassName } vnode =
  if hasClassName startClassName vnode && (not $ hasClassName activeClassName vnode)
    then addClassName activeClassName vnode
    else vnode

willEnterVNodes
  :: forall f state
   . Config
  -> Array (VNode f state)
  -> Array (VNode f state)
  -> Tuple (Array (VNode f state)) (Array (VNode f state))
willEnterVNodes config prevs currents =
  case config.enter of
    Nothing -> Tuple currents []
    Just { startClassName } ->
      let addFirstClassName acc vnode =
            if isIncluded prevs vnode || hasClassName startClassName vnode
              then pure acc
              else do
                tell [ vnode ]
                pure $ modifyAtIndices
                  [ unsafePartial $ fromJust $ findIndex (equalKey vnode) acc ]
                  (addClassName startClassName)
                  acc
       in runWriter $ foldRecM addFirstClassName currents currents

willLeaveVNodes
  :: forall f state
   . Config
  -> Array (VNode f state)
  -> Array (VNode f state)
  -> Tuple (Array (VNode f state)) (Array (VNode f state))
willLeaveVNodes config prevs currents =
  case config.leave of
    Nothing -> Tuple currents []
    Just { startClassName } ->
      let addFirstClassName acc vnode =
            if isIncluded currents vnode || hasClassName startClassName vnode
              then pure acc
              else do
                tell [ vnode ]
                pure $ unsafePartial $ fromJust $ insertAt
                  (unsafePartial $ fromJust $ findIndex (equalKey vnode) prevs)
                  (addClassName startClassName vnode)
                  acc
       in runWriter $ foldRecM addFirstClassName currents prevs

hasClassName :: forall f state. String -> VNode f state -> Boolean
hasClassName name vnode =
  case vnode of
    VNode _ (Element r) -> has r
    VNode _ (OperativeElement _ r) -> has r
    _ -> true
  where
    has :: forall m. VObject f state m -> Boolean
    has r =
      case lookup "className" r.props of
        Nothing -> false
        Just val ->
          elem name $ split (Pattern " ") val

addClassName :: forall f state. String -> VNode f state -> VNode f state
addClassName name vnode =
  case vnode of
    VNode k (Element r) -> VNode k $ Element $ add r
    VNode k (OperativeElement bf r) -> VNode k $ OperativeElement bf $ add r
    _ -> vnode
  where
    add :: forall m. VObject f state m -> VObject f state m
    add r = r { props = alter add' "className" r.props }

    add' current =
      case current of
        Nothing -> Just name
        Just val ->
          Just $ joinWith " " $ snoc (split (Pattern " ") val) name

removeClassName :: forall f state. String -> VNode f state -> VNode f state
removeClassName name vnode =
  case vnode of
    VNode k (Element r) -> VNode k $ Element $ remove r
    VNode k (OperativeElement bf r) -> VNode k $ OperativeElement bf $ remove r
    _ -> vnode
  where
    remove :: forall m. VObject f state m -> VObject f state m
    remove r = r { props = update remove' "className" r.props }

    remove' val =
      let classes = filter (_ /= name) $ split (Pattern " ") val
       in if length classes <= 0 then Nothing else Just $ joinWith " " classes

isIncluded
  :: forall f state
   . Array (VNode f state)
  -> VNode f state
  -> Boolean
isIncluded vnodes vnode =
  any (equalKey vnode) vnodes

equalKey
  :: forall f state
   . VNode f state
  -> VNode f state
  -> Boolean
equalKey a b = key a == key b
