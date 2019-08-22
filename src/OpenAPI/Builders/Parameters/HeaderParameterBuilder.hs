{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module OpenAPI.Builders.Parameters.HeaderParameterBuilder
  ( configHeaderParameter
  , nameHeaderParameter
  , refHeaderParameter
  , requiredHeaderParameter
  , deprecatedHeaderParameter
  , allowEmptyValueHeaderParameter
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((.~), (?~))
import Lens.Micro.TH
import OpenAPI.Errors
import OpenAPI.Types
import OpenAPI.Utils

type HeaderParameterBuilder = State HeaderParameterB ()

data HeaderParameterB = HeaderParameterB
  { _headerParameterNameB            :: Text
  , _headerParameterDescriptionB     :: Maybe Text
  , _headerParameterRequiredB        :: Bool
  , _headerParameterDeprecatedB      :: Bool
  , _headerParameterAllowEmptyValueB :: Bool
  } deriving (Eq, Show)

$(makeLenses ''HeaderParameterB)

configHeaderParameter :: HeaderParameterBuilder -> Either ParameterErr HeaderParameter
configHeaderParameter = convertP . flip execState emptyHeaderParameterB

convertP :: HeaderParameterB -> Either ParameterErr HeaderParameter
convertP (HeaderParameterB n d r dep e) | emptyTxt n = Left InvalidNameParameter
                                       | emptyTxtMaybe d = Left InvalidDescriptionParameter
                                       | otherwise = pure $ HeaderParameter n d r dep e

nameHeaderParameter :: Text -> HeaderParameterBuilder
nameHeaderParameter n = modify $ headerParameterNameB .~ n

refHeaderParameter :: Text -> HeaderParameterBuilder
refHeaderParameter r = modify $ headerParameterDescriptionB ?~ r

requiredHeaderParameter :: HeaderParameterBuilder
requiredHeaderParameter = modify $ headerParameterRequiredB .~ True

deprecatedHeaderParameter :: HeaderParameterBuilder
deprecatedHeaderParameter = modify $ headerParameterDeprecatedB .~ True

allowEmptyValueHeaderParameter :: HeaderParameterBuilder
allowEmptyValueHeaderParameter = modify $ headerParameterAllowEmptyValueB .~ True

emptyHeaderParameterB :: HeaderParameterB
emptyHeaderParameterB = HeaderParameterB "" Nothing False False False
