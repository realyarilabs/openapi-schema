{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell #-}

module Types where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (isNothing)
import           Data.Text (Text, pack, toLower)
import           Lens.Micro.TH
import           Utils (pairMaybes)

data OpenAPI = OpenAPI
  { _openAPI   :: Text
  , _openInfo  :: Info
  , _openPaths :: [Path]
  } deriving (Eq, Show)

data Info = Info
  { _infoTitle          :: Text
  , _infoDescription    :: Maybe Text
  , _infoTermsOfService :: Maybe Text
  , _infoContact        :: Maybe Contact
  , _infoLicense        :: Maybe License
  , _infoVersion        :: Text
  } deriving (Eq, Show)

data Path = Path
  { _pathName        :: Text
  , _pathSummary     :: Maybe Text
  , _pathDescription :: Maybe Text
  , _pathOperations  :: [Operation]
  } deriving (Eq, Show)

data Operation = Operation
  { _operationType        :: OperationType
  , _operationTags        :: Maybe [Text]
  , _operationSummary     :: Maybe Text
  , _operationDescription :: Maybe Text
  , _operationResponses   :: [Responses]
  } deriving (Eq, Show)

data Responses  = Default Response
                | Status Text Response
                deriving (Eq, Show)

data Response = Response -- add json
  { _responseDescription   :: Text
  }
  deriving (Eq, Show)

data OperationType = GET
                   | POST
                   | PUT
                   | DELETE
                   | OPTIONS
                   | HEAD
                   | PATCH
                   | TRACE
                   deriving (Eq, Show, Ord)

data Contact = Contact
  { _contactName  :: Maybe Text
  , _contactUrl   :: Maybe Text
  , _contactEmail :: Maybe Text
  } deriving (Eq, Show)

data License = License
  { _licenseName :: Text
  , _licenseUrl  :: Maybe Text
  } deriving (Eq, Show)



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

makeLenses ''OpenAPI
makeLenses ''Info
makeLenses ''Path
makeLenses ''Operation
makeLenses ''Responses
makeLenses ''Response
makeLenses ''Contact
makeLenses ''License
