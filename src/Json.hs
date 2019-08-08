{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Json where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (isNothing)
import           Data.Text (Text, pack, toLower)
import           Types
import           Utils (pairMaybes)


instance ToJSON Response where
  toJSON Response{..} = Object $ "description" .= _responseDescription

instance ToJSON Operation where
  toJSON Operation{..} =  object $
    pairMaybes [_operationDescription, _operationSummary]
                ["description", "summary"]
    <>
    pairMaybes [_operationTags] ["tags"]
    <>
    ["responses" .=  HM.fromList (foldr getPair [] _operationResponses) ] where
      getPair (Default r)  a = ("default",r):a
      getPair (Status s r) a = (s,r):a

instance ToJSON Path where
  toJSON Path{..} = object $
    pairMaybes [_pathSummary, _pathDescription] ["summary", "description"]
    <>
    foldr (\Operation{..} a ->
            ((toLower . pack . show) _operationType .= Operation{..}):a)
          [] _pathOperations

instance ToJSON Contact where
  toJSON Contact{..} = object $
    pairMaybes [_contactName, _contactEmail, _contactUrl]
                ["name", "email" ,"url"]

instance ToJSON License where
  toJSON License{..} = object $
    ["name" .= _licenseName]
    <>
    pairMaybes [_licenseUrl] ["url"]

instance ToJSON Info where
  toJSON Info{..} = object $
    ["title" .= _infoTitle, "version" .= _infoVersion]
    <>
    pairMaybes [_infoContact] ["contact"]
    <>
    pairMaybes [_infoLicense] ["license"]
    <>
    pairMaybes [_infoDescription, _infoTermsOfService]
                ["description", "termsOfService"]

instance ToJSON OpenAPI where
  toJSON OpenAPI{..} = object
    ["openapi" .= _openAPI, "info" .= _openInfo
    ,"paths" .= HM.fromList (foldr (\Path{..} a -> (_pathName, Path{..}):a)
                                   [] _openPaths :: [(Text, Path)]
                            )
    ]
