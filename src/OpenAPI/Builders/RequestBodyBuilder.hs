{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.RequestBodyBuilder
  ( configRequestBody
  , contentRequestBody
  , requiredRequestBody
  , descriptionRequestBody
  , RequestBodyBuilder
  ) where

import           Control.Monad.State (State, execState, modify)
import           Data.Bifunctor (first)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           Lens.Micro ((%~), (.~), (?~))
import           Lens.Micro.TH
import           OpenAPI.Errors
import           OpenAPI.Types
import           OpenAPI.Utils

type RequestBodyBuilder = State RequestBodyB ()

data RequestBodyB = RequestBodyB
  { _requestBodyDescriptionB :: Maybe Text
  , _requestBodyContentB     :: HashMap Text (Either MediaTypeErr MediaType)
  , _requestBodyRequiredB    :: Bool
  } deriving (Eq, Show)

$(makeLenses ''RequestBodyB)

configRequestBody :: RequestBodyBuilder -> Either RequestBodyErr RequestBody
configRequestBody = convertC . flip execState emptyRequestBodyB

convertC :: RequestBodyB -> Either RequestBodyErr RequestBody
convertC (RequestBodyB d c r) | emptyTxtMaybe d = Left InvalidDescriptionRequestBody
                              | otherwise = do
                                content <- first InvalidMediaR . sequence $ c
                                pure $ RequestBody d content r

descriptionRequestBody :: Text -> RequestBodyBuilder
descriptionRequestBody d = modify $ requestBodyDescriptionB ?~ d

contentRequestBody :: Text -> Either MediaTypeErr MediaType -> RequestBodyBuilder
contentRequestBody k v = modify $ requestBodyContentB %~ HM.insert k v

requiredRequestBody :: RequestBodyBuilder
requiredRequestBody = modify $ requestBodyRequiredB .~ True


emptyRequestBodyB :: RequestBodyB
emptyRequestBodyB = RequestBodyB Nothing HM.empty False
