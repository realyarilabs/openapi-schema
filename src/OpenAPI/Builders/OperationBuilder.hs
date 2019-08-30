{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.OperationBuilder
  ( configOperation
  , tagOperation
  , summaryOperation
  , descriptionOperation
  , typeOperation
  , statusResponseOperation
  , statusResponseOperationRef
  , defaultResponseOperation
  , defaultResponseOperationRef
  , docsOperation
  , idOperation
  , parameterOperation
  , parameterRefOperation
  , deprecatedOperation
  , securityOperation
  , serverOperation
  , requestBodyOperation
  , requestBodyRefOperation
  , OperationBuilder
  ) where

import           Control.Monad.State (State, execState, modify)
import           Data.Bifunctor (bimap, first)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           Lens.Micro
import           Lens.Micro.TH
import           OpenAPI.Errors
import           OpenAPI.Types
import           OpenAPI.Utils

type OperationBuilder = State OperationB ()

data OperationB = OperationB
  { _operationTypeB        :: Either () OperationType
  , _operationTagsB        :: [Text]
  , _operationSummaryB     :: Maybe Text
  , _operationDescriptionB :: Maybe Text
  , _operationResponsesB   :: HashMap Text (Either ResponsesErr Responses)
  , _operationDocsB        :: Maybe (Either ExternalDocsErr ExternalDocs)
  , _operationIdB          :: Maybe Text
  , _operationParametersB  :: [MkRef (Either ParameterErr Parameter) (Either ReferenceErr Reference)]
  , _operationRequestBodyB :: Maybe (MkRef (Either RequestBodyErr RequestBody) (Either ReferenceErr Reference))
  , _operationDeprecatedB  :: Bool
  , _operationSecurityB    :: [Either SecReqErr SecReq]
  , _operationServersB     :: [Either ServerErr Server]
  } deriving (Eq, Show)

$(makeLenses ''OperationB)


configOperation :: OperationBuilder -> Either OperationErr Operation
configOperation = convertO . flip execState emptyOperationB

convertO :: OperationB -> Either OperationErr Operation
convertO OperationB{_operationTypeB = (Left _)} = Left InvalidType
convertO (OperationB (Right typ) tags su desc resp docs idO pa rb dep sec serv) | emptyTxts tags = Left InvalidTags
                                          | HM.null resp = Left NoResponses
                                          | notElem "default" . HM.keys $ resp = Left NoDefault
                                          | emptyTxtMaybe su = Left InvalidSummaryO
                                          | emptyTxtMaybe desc = Left InvalidDescriptionO
                                          | emptyTxtMaybe idO = Left InvalidIdO
                                          | isJust . foldRefProds $ pa = Left InvalidParameterO
                                          | otherwise = do
                                            res <- first InvalidResponses . sequence $ resp
                                            security <- first InvalidSecurityO . sequence $ sec
                                            servers <- first InvalidServerO . sequence $ serv
                                            docsE <-  first InvalidDocsO . sequence $ docs
                                            reqb <- maybe (pure Nothing) foldRequest rb
                                            let para = fmap (bimap (^?!_Right) (^?!_Right)) pa -- partial, checked in the last guard
                                            pure $ Operation typ tags su desc res docsE idO para reqb dep security servers


foldRequest :: MkRef (Either RequestBodyErr RequestBody) (Either ReferenceErr Reference) -> Either OperationErr (Maybe (Referenceable RequestBody))
foldRequest (MkRef (Left (Left e)))   = Left . InvalidRequestBodyO $ e
foldRequest (MkRef (Right (Left e)))  = Left . InvalidRequestBodyRefO $ e
foldRequest (MkRef (Right (Right e))) = pure . pure . MkRef . pure $ e
foldRequest (MkRef (Left (Right e)))  = pure . pure . MkRef . Left $ e


typeOperation :: OperationType -> OperationBuilder
typeOperation t = modify $ operationTypeB .~ pure t

tagOperation :: Text -> OperationBuilder
tagOperation o = modify $ operationTagsB %~ (o:)

summaryOperation :: Text -> OperationBuilder
summaryOperation s = modify $ operationSummaryB ?~ s

descriptionOperation :: Text -> OperationBuilder
descriptionOperation d = modify $ operationDescriptionB ?~ d

defaultResponseOperation :: Either ResponseErr Response -> OperationBuilder
defaultResponseOperation r = modify $ operationResponsesB %~ HM.insert "default" (bimap InvalidResponse ResponsesR r)

defaultResponseOperationRef :: Either ReferenceErr Reference -> OperationBuilder
defaultResponseOperationRef r = modify $ operationResponsesB %~ HM.insert "default" (bimap InvalidReferenceR ResponsesRef r)

statusResponseOperation :: Text -> Either ResponseErr Response -> OperationBuilder
statusResponseOperation "" _             = modify $ operationResponsesB %~ HM.insert "" (Left (InvalidResponse InvalidHttpStatus)) -- better test needed
statusResponseOperation status (Right e) = modify $ operationResponsesB %~ HM.insert status (pure (ResponsesR e))
statusResponseOperation status (Left e)  = modify $ operationResponsesB %~ HM.insert status (Left (InvalidResponse e))

statusResponseOperationRef :: Text -> Either ReferenceErr Reference -> OperationBuilder
statusResponseOperationRef "" _             = modify $ operationResponsesB %~ HM.insert "" (Left (InvalidResponse InvalidHttpStatus)) -- better test needed
statusResponseOperationRef status (Right e) = modify $ operationResponsesB %~ HM.insert status (pure (ResponsesRef e))
statusResponseOperationRef status (Left e)  = modify $ operationResponsesB %~ HM.insert status (Left (InvalidReferenceR e))

docsOperation :: Either ExternalDocsErr ExternalDocs -> OperationBuilder
docsOperation docs = modify $ operationDocsB ?~ docs

idOperation :: Text -> OperationBuilder
idOperation i = modify $ operationIdB ?~ i

parameterOperation :: Either ParameterErr Parameter -> OperationBuilder
parameterOperation p = modify $ operationParametersB %~ (MkRef (Left p):)


parameterRefOperation :: Either ReferenceErr Reference -> OperationBuilder
parameterRefOperation p = modify $ operationParametersB %~ (MkRef (pure p):)

deprecatedOperation :: OperationBuilder
deprecatedOperation = modify $ operationDeprecatedB .~ True

securityOperation :: Either SecReqErr SecReq -> OperationBuilder
securityOperation s = modify $ operationSecurityB %~ (s:)

serverOperation :: Either ServerErr Server -> OperationBuilder
serverOperation s = modify $ operationServersB %~ (s:)

requestBodyOperation :: Either RequestBodyErr RequestBody -> OperationBuilder
requestBodyOperation r = modify $ operationRequestBodyB ?~ MkRef (Left r)

requestBodyRefOperation :: Either ReferenceErr Reference -> OperationBuilder
requestBodyRefOperation r = modify $ operationRequestBodyB ?~ MkRef (Right r)

emptyOperationB :: OperationB
emptyOperationB = OperationB (Left ()) [] Nothing Nothing HM.empty Nothing Nothing [] Nothing False [] []
