{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module OpenAPI.Builders.ParameterBuilder
  ( configParameter
  , nameParameter
  , refParameter
  , requiredParameter
  , typeParameter
  , deprecatedParameter
  , allowEmptyValueParameter
  , schemaParameter
  , schemaRefParameter
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Bifunctor
import Data.Text (Text)
import Lens.Micro
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
  , _parameterSchemaB          :: MkRef (Either SchemaErr Schema) (Either ReferenceErr Reference)
  } deriving (Eq, Show)

$(makeLenses ''ParameterB)

configParameter :: ParameterBuilder -> Either ParameterErr Parameter
configParameter = convertP . flip execState emptyParameterB

convertP :: ParameterB -> Either ParameterErr Parameter
convertP ParameterB{_parameterInB = (Left _)} = Left InvalidTypeParameter
convertP ParameterB{_parameterSchemaB = (MkRef (Left (Left e)))} = Left . InvalidSchemaParameter $ e
convertP ParameterB{_parameterSchemaB = (MkRef (Right (Left e)))} = Left . InvalidSchemaRefParameter $ e
convertP (ParameterB n (Right t) d r dep e s) | emptyTxt n = Left InvalidNameParameter
                                              | emptyTxtMaybe d = Left InvalidDescriptionParameter
                                              | t == PATH = pure $ Parameter n t d True dep e (bimap (^?!_Right) (^?!_Right) s)
                                              | otherwise = pure $ Parameter n t d r dep e (bimap (^?!_Right) (^?!_Right) s)

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

schemaRefParameter :: Either ReferenceErr Reference -> ParameterBuilder
schemaRefParameter r = modify $ parameterSchemaB .~ (MkRef . Right) r

schemaParameter :: Either SchemaErr Schema -> ParameterBuilder
schemaParameter s = modify $ parameterSchemaB .~ (MkRef . Left) s

emptyParameterB :: ParameterB
emptyParameterB = ParameterB "" (Left ()) Nothing False False False (MkRef (Left (Left InvalidExampleSchema)))
