{-# LANGUAGE DeriveTraversable, DerivingVia, OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module OpenAPI.Types where

import           Control.Monad (join)
import           Data.Aeson
import           Data.Bifunctor
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (isNothing)
import           Data.Text (Text, pack, toLower)
import           Lens.Micro.TH

newtype MkRef a b = MkRef (Either a b)
  deriving Bifunctor via Either
  deriving (Functor, Applicative, Foldable, Monad) via Either a
  deriving (Eq, Show) via (Either a b)
  deriving stock  Traversable

type Referenceable a = MkRef a Reference

instance (ToJSON b, ToJSON a) => ToJSON (MkRef a b) where
  toJSON (MkRef (Left x))  = toJSON x
  toJSON (MkRef (Right x)) = toJSON x

data OpenAPI = OpenAPI
  { _openAPI      :: Text
  , _openInfo     :: Info
  , _openSecurity :: [SecReq]
  , _openPaths    :: [Path]
  , _openServers  :: [Server]
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
  , _pathRef         :: Maybe Text
  , _pathSummary     :: Maybe Text
  , _pathDescription :: Maybe Text
  , _pathParameters  :: [Referenceable Parameter]
  , _pathServers     :: [Server]
  , _pathOperations  :: [Operation]
  } deriving (Eq, Show)

data Operation = Operation
  { _operationType        :: OperationType
  , _operationTags        :: [Text]
  , _operationSummary     :: Maybe Text
  , _operationDescription :: Maybe Text
  , _operationResponses   :: HashMap Text Responses
  , _operationDocs        :: Maybe ExternalDocs
  , _operationId          :: Maybe Text
  , _operationParameters  :: [Referenceable Parameter]
  -- , _operationRequestBody :: Referenceable RequestBody
  , _operationDeprecated  :: Bool
  , _operationSecurity    :: [SecReq]
  , _operationServers     :: [Server]
  } deriving (Eq, Show)

data Responses = ResponsesR Response
               | ResponsesRef Reference
               deriving (Eq, Show)

data Response = Response
  { _responseDescription :: Text
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

data Server = Server
  { _serverURL         :: Text
  , _serverDescription :: Maybe Text
  , _serverVars        :: HashMap Text ServerVar
  } deriving (Eq, Show)

data ServerVar = ServerVar
  { _serverVEnum        :: [Text]
  , _serverVDefault     :: Text
  , _serverVDescription :: Maybe Text
  } deriving (Eq, Show)

data SecReq = SecReq
  { _secReqName  :: Text
  , _secReqScope :: [Text]
  } deriving (Eq, Show)

data Reference = Reference
  { _refReference  :: Text
  } deriving (Eq, Show)

data ParameterType = HEADER
                   | QUERY
                   | PATH
                   | COOKIE
                   deriving (Eq, Show)

data Parameter = Parameter
  { _parameterName            :: Text
  , _parameterIn              :: ParameterType
  , _parameterDescription     :: Maybe Text
  , _parameterRequired        :: Bool
  , _parameterDeprecated      :: Bool
  , _parameterAllowEmptyValue :: Bool
  , _parameterSchema          :: Referenceable Schema
  } deriving (Eq, Show)

data Schema = Schema
  { _schemaNullabe       :: Bool
  , _schemaDiscriminator :: Discriminator
  , _schemaReadOnly      :: Bool
  , _schemaWriteOnly     :: Bool
  , _schemaXml           :: Xml
  , _schemaExternalDocs  :: ExternalDocs
  , _schemaExample       :: Maybe Text
  , _schemaDeprecated    :: Bool
  } deriving (Eq, Show)

data Discriminator = Discriminator
  { _discriminatorPropertyName :: Text
  , _discriminatorMapping      :: HashMap Text Text
  } deriving (Eq, Show)

data Xml = Xml
  { _xmlName      :: Text
  , _xmlNameSpace :: Text
  , _xmlPrefix    :: Text
  , _xmlAttribute :: Bool
  , _xmlWrapped   :: Bool
  } deriving (Eq, Show)

data ExternalDocs = ExternalDocs
  { _externalDocsDescription :: Maybe Text
  , _externalDocsURL         :: Text
  } deriving (Eq, Show)

$(makeLenses ''OpenAPI)
$(makeLenses ''Info)
$(makeLenses ''Path)
$(makeLenses ''Operation)
$(makeLenses ''Responses)
$(makeLenses ''Response)
$(makeLenses ''Contact)
$(makeLenses ''License)
$(makeLenses ''ServerVar)
$(makeLenses ''SecReq)
$(makeLenses ''Reference)
$(makeLenses ''Parameter)



instance ToJSON Reference where
  toJSON Reference{..} = Object $ "$ref" .= _refReference

instance ToJSON SecReq where
  toJSON SecReq{..} = Object $ _secReqName .= _secReqScope

instance ToJSON Response where
  toJSON Response{..} = Object $ "description" .= _responseDescription

instance ToJSON Responses where
  toJSON (ResponsesR r)   = toJSON r
  toJSON (ResponsesRef r) = toJSON r

instance ToJSON Operation where
  toJSON Operation{..} =  object $
    pairMaybes [_operationDescription, _operationSummary] ["description", "summary"] 
    <>
    pairMaybes [_operationDocs] ["externalDocs"]
    <>
    ["tags" .= _operationTags]
    <>
    ["responses" .=  _operationResponses ]
    <>
    ["operationId" .= _operationId]
    <>
    ["parameters" .= _operationParameters, "security" .= _operationSecurity, "servers" .= _operationServers]
    <>
    ["deprecated" .= _operationDeprecated]

instance ToJSON Path where
  toJSON Path{..} = object $
    pairMaybes [_pathSummary, _pathDescription, _pathRef] ["summary", "description", "$ref"]
    <>
    foldr (\Operation{..} a ->
            ((toLower . pack . show) _operationType .= Operation{..}):a)
          [] _pathOperations
    <>
    ["servers" .= _pathServers]
    <>
    ["parameters" .= _pathParameters]

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
    , "servers" .= _openServers
    , "security" .= _openSecurity
    ]

instance ToJSON ServerVar where
  toJSON ServerVar{..} = object $
    [ "default" .= _serverVDefault
    , "enum" .= _serverVEnum
    ]
    <>
    pairMaybes [_serverVDescription] ["description"]

instance ToJSON Server where
  toJSON Server{..} = object $
    ["url" .= _serverURL]
    <>
    pairMaybes [_serverDescription] ["description"]
    <>
    ["variables" .= _serverVars]

instance ToJSON Parameter where
  toJSON Parameter{..} = object $
    ["name" .= _parameterName, "in" .= _parameterIn]
    <>
    pairMaybes [_parameterDescription] ["description"]
    <>
    ["required" .= _parameterRequired, "deprecated" .= _parameterDeprecated]
    <>
    ["allowEmptyValue" .= _parameterAllowEmptyValue]
    <>
    ["schema" .= _parameterSchema]

instance ToJSON ParameterType where
  toJSON HEADER = String "header"
  toJSON QUERY  = String "query"
  toJSON PATH   = String "path"
  toJSON COOKIE = String "cookie"

instance ToJSON Schema where
  toJSON Schema{..} = object $
    ["nullable" .= _schemaNullabe]
    <>
    ["readOnly" .= _schemaReadOnly, "writeOnly" .= _schemaWriteOnly]
    <>
    ["deprecated" .= _schemaDeprecated]
    <>
    ["discriminator" .= _schemaDiscriminator, "xml" .= _schemaDiscriminator]
    <>
    ["externalDocs" .= _schemaExternalDocs]
    <>
    pairMaybes [_schemaExample] ["example"]

instance ToJSON Discriminator where
  toJSON Discriminator{..} = object $
    ["propertyName" .= _discriminatorPropertyName, "mapping" .= _discriminatorMapping]

instance ToJSON Xml where
  toJSON Xml{..} = object $
    ["name" .= _xmlName, "namespace" .= _xmlNameSpace, "prefix" .= _xmlPrefix]
    <>
    ["attribute" .= _xmlAttribute, "wrapped" .= _xmlWrapped]

instance ToJSON ExternalDocs where
  toJSON ExternalDocs{..} = object $
    ["url" .= _externalDocsURL]
    <>
    pairMaybes [_externalDocsDescription] ["description"]

{-
   Some records have optional fields, this are represented by a `Maybe a`. When
   generating the JSON representation we do not want to have fields with the
   `Nothing` value, since this is transpiled to a `null`.
   This function removes all the `Nothing`s and generates the respective
   `Pair`s (Name, Value) to be converted JSON.
-}
pairMaybes :: (ToJSON a, KeyValue b) => [Maybe a] -> [Text] -> [b]
pairMaybes a b = join $ zipWith (\x y -> if isNothing x then [] else [y .= x]) a b

