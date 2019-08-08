{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.ResponseBuilder
  ( configResponse
  , descriptionResponse
  , ResponseBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((.~))
import Lens.Micro.TH
import OpenAPI.Errors (ResponseErr (..))
import OpenAPI.Types (Response (..))


type ResponseBuilder = State ResponseB ()

data ResponseB = ResponseB
  { _responseDescriptionB  :: Text
  } deriving (Eq, Show)

$(makeLenses ''ResponseB)

configResponse :: ResponseBuilder -> Either ResponseErr Response
configResponse = convertR . flip execState emptyResponseB

convertR :: ResponseB -> Either ResponseErr Response
convertR (ResponseB "") = Left InvalidDescriptionR
convertR (ResponseB d)  = Right $ Response d


descriptionResponse :: Text -> ResponseBuilder
descriptionResponse c = modify $ responseDescriptionB .~ c


emptyResponseB :: ResponseB
emptyResponseB = ResponseB ""
