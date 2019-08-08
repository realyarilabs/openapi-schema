{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Text (Text)
import Lens.Micro.TH

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


makeLenses ''OpenAPI
makeLenses ''Info
makeLenses ''Path
makeLenses ''Operation
makeLenses ''Responses
makeLenses ''Response
makeLenses ''Contact
makeLenses ''License
