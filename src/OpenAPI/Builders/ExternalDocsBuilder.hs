{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.ExternalDocsBuilder
  ( configExternalDocs
  , descriptionExternalDocs
  , urlExternalDocs
  , ExternalDocsBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((.~), (?~))
import Lens.Micro.TH
import OpenAPI.Errors (ExternalDocsErr (..))
import OpenAPI.Types (ExternalDocs (..))
import OpenAPI.Utils


type ExternalDocsBuilder = State ExternalDocsB ()

data ExternalDocsB = ExternalDocsB
  { _externalDocsDescriptionB :: Maybe Text
  , _externalDocsURLB         :: Text
  } deriving (Eq, Show)

$(makeLenses ''ExternalDocsB)

configExternalDocs :: ExternalDocsBuilder -> Either ExternalDocsErr ExternalDocs
configExternalDocs = convertC . flip execState emptyExternalDocsB

convertC :: ExternalDocsB -> Either ExternalDocsErr ExternalDocs
convertC (ExternalDocsB d url ) | emptyTxtMaybe d = Left InvalidDescriptionDocs
                                | not . isValidURL $ url = Left InvalidURLDocs
                                | otherwise = pure $ ExternalDocs d url

descriptionExternalDocs :: Text -> ExternalDocsBuilder
descriptionExternalDocs d = modify $ externalDocsDescriptionB ?~ d

urlExternalDocs :: Text -> ExternalDocsBuilder
urlExternalDocs url = modify $ externalDocsURLB .~ url


emptyExternalDocsB :: ExternalDocsB
emptyExternalDocsB = ExternalDocsB Nothing ""
