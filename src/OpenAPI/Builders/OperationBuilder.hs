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
  , OperationBuilder
  ) where

import           Control.Monad.State (State, execState, modify)
import           Data.Bifunctor (bimap)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           Lens.Micro ((%~), (.~), (?~))
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
  } deriving (Eq, Show)

$(makeLenses ''OperationB)

configOperation :: OperationBuilder -> Either OperationErr Operation
configOperation = convertO . flip execState emptyOperationB

convertO :: OperationB -> Either OperationErr Operation
convertO (OperationB (Left _) _ _ _ _)    = Left InvalidType
convertO (OperationB (Right t) ts s d rs) | emptyTxts ts = Left InvalidTags
                                          | HM.null rs = Left NoResponses
                                          | notElem "default" . HM.keys $ rs = Left NoDefault
                                          | emptyTxtMaybe s = Left InvalidSummaryO
                                          | emptyTxtMaybe d = Left InvalidDescriptionO
                                          | otherwise = foldBuilder InvalidResponses (Operation t ts s d) rs

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

emptyOperationB :: OperationB
emptyOperationB = OperationB (Left ()) [] Nothing Nothing HM.empty
