module Example.Hello where

import Example.Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Example.Mathjax as MathJax
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Nonbili.DOM as NDOM


type State =
  { count :: Int }

data Action
  = CountUp
  | Initialized

type ChildSlots = ()

formulaRef = H.RefLabel "formula" :: H.RefLabel

component :: forall q o m. MonadEffect m => H.Component HH.HTML q Unit o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialized
      }
    }
  where

  initialState :: State
  initialState =
    { count: 0 }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = HH.div_
    [ HH.span
      [ HP.ref formulaRef]
      []
    , HH.button [ HE.onClick (\_ -> Just CountUp)] [HH.text "+1"]
    ]

  updateFormula :: H.HalogenM State Action ChildSlots o m Unit
  updateFormula = do
    state <- H.get
    H.getHTMLElementRef formulaRef >>= traverse_ \el ->
      H.liftEffect $ NDOM.setInnerText el $ "\\( count = " <> show state.count <> " \\)"
    liftEffect $ MathJax.typeset

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    CountUp -> do
      H.modify_ (\st -> st { count = st.count + 1})
      updateFormula
    Initialized ->
      updateFormula
