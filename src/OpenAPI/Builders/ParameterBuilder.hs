{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module OpenAPI.Builders.ParameterBuilder
  ( configParameter
  , nameParameter
  , refParameter
  , requiredParameter
  , typeParameter
  , deprecatedParameter
  , allowEmptyValueParameter
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((.~), (?~))
import Lens.Micro.TH
import OpenAPI.Errors
import OpenAPI.Types
import OpenAPI.Utils

type ParameterBuilder = State ParameterB ()

data ParameterB = ParameterB
  { _parameterNameB            :: Text
  , _parameterInB              :: Either () ParameterType
  , _parameterDescriptionB     :: Maybe Text
  , _parameterRequiredB        :: Bool
  , _parameterDeprecatedB      :: Bool
  , _parameterAllowEmptyValueB :: Bool
  } deriving (Eq, Show)

$(makeLenses ''ParameterB)

configParameter :: ParameterBuilder -> Either ParameterErr Parameter
configParameter = convertP . flip execState emptyParameterB

convertP :: ParameterB -> Either ParameterErr Parameter
convertP (ParameterB _ (Left _) _ _ _ _)    = Left InvalidTypeParameter
convertP (ParameterB n (Right t) d r dep e) | emptyTxt n = Left InvalidNameParameter
                                            | emptyTxtMaybe d = Left InvalidDescriptionParameter
                                            | t == PATH = pure $ Parameter n t d True dep e
                                            | otherwise = pure $ Parameter n t d r dep e

nameParameter :: Text -> ParameterBuilder
nameParameter n = modify $ parameterNameB .~ n

typeParameter :: ParameterType -> ParameterBuilder
typeParameter t = modify $ parameterInB .~ pure t

refParameter :: Text -> ParameterBuilder
refParameter r = modify $ parameterDescriptionB ?~ r

requiredParameter :: ParameterBuilder
requiredParameter = modify $ parameterRequiredB .~ True

deprecatedParameter :: ParameterBuilder
deprecatedParameter = modify $ parameterDeprecatedB .~ True

allowEmptyValueParameter :: ParameterBuilder
allowEmptyValueParameter = modify $ parameterAllowEmptyValueB .~ True

emptyParameterB :: ParameterB
emptyParameterB = ParameterB "" (Left ()) Nothing False False False
