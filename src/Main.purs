module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff)
import Example.Hello as ExpHello
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "Hello" $ proxy ExpHello.component
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>= runStorybook
    { stories
    , logo: Nothing
    }
