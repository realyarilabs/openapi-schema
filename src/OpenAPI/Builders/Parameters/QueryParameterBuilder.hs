{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module OpenAPI.Builders.Parameters.QueryParameterBuilder
  ( configQueryParameter
  , nameQueryParameter
  , refQueryParameter
  , requiredQueryParameter
  , deprecatedQueryParameter
  , allowEmptyValueQueryParameter
  )where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((.~), (?~))
import Lens.Micro.TH
import OpenAPI.Errors
import OpenAPI.Types
import OpenAPI.Utils

type QueryParameterBuilder = State QueryParameterB ()

data QueryParameterB = QueryParameterB
  { _queryParameterNameB            :: Text
  , _queryParameterDescriptionB     :: Maybe Text
  , _queryParameterRequiredB        :: Bool
  , _queryParameterDeprecatedB      :: Bool
  , _queryParameterAllowEmptyValueB :: Bool
  } deriving (Eq, Show)

$(makeLenses ''QueryParameterB)

configQueryParameter :: QueryParameterBuilder -> Either ParameterErr QueryParameter
configQueryParameter = convertP . flip execState emptyQueryParameterB

convertP :: QueryParameterB -> Either ParameterErr QueryParameter
convertP (QueryParameterB n d r dep e) | emptyTxt n = Left InvalidNameParameter
                                       | emptyTxtMaybe d = Left InvalidDescriptionParameter
                                       | otherwise = pure $ QueryParameter n d r dep e

nameQueryParameter :: Text -> QueryParameterBuilder
nameQueryParameter n = modify $ queryParameterNameB .~ n

refQueryParameter :: Text -> QueryParameterBuilder
refQueryParameter r = modify $ queryParameterDescriptionB ?~ r

requiredQueryParameter :: QueryParameterBuilder
requiredQueryParameter = modify $ queryParameterRequiredB .~ True

deprecatedQueryParameter :: QueryParameterBuilder
deprecatedQueryParameter = modify $ queryParameterDeprecatedB .~ True

allowEmptyValueQueryParameter :: QueryParameterBuilder
allowEmptyValueQueryParameter = modify $ queryParameterAllowEmptyValueB .~ True

emptyQueryParameterB :: QueryParameterB
emptyQueryParameterB = QueryParameterB "" Nothing False False False
