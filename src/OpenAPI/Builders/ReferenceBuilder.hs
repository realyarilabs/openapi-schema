{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.ReferenceBuilder
  ( configReference
  , referenceRef
  , ReferenceBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((.~))
import Lens.Micro.TH
import OpenAPI.Errors (ReferenceErr (..))
import OpenAPI.Types (Reference (..))
import OpenAPI.Utils


type ReferenceBuilder = State ReferenceB ()

data ReferenceB = ReferenceB
  { _refReferenceB  :: Text
  } deriving (Eq, Show)

$(makeLenses ''ReferenceB)

configReference :: ReferenceBuilder -> Either ReferenceErr Reference
configReference = convertC . flip execState emptyReferenceB

convertC :: ReferenceB -> Either ReferenceErr Reference
convertC (ReferenceB r) | emptyTxt r = Left InvalidEmptyReference
                        | otherwise = pure $ Reference r

referenceRef :: Text -> ReferenceBuilder
referenceRef r = modify $ refReferenceB .~ r

emptyReferenceB :: ReferenceB
emptyReferenceB = ReferenceB ""
