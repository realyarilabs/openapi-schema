{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.OperationBuilder
  ( configOperation
  , tagOperation
  , summaryOperation
  , descriptionOperation
  , typeOperation
  , statusResponseOperation
  , defaultResponseOperation
  , OperationBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Either (isLeft, lefts, rights)
import Data.Maybe (maybe)
import Data.Text (Text)
import Lens.Micro ((%~), (.~), (?~))
import Lens.Micro.TH
import OpenAPI.Errors (OperationErr (..), ResponseErr (..))
import OpenAPI.Types
import OpenAPI.Utils

type OperationBuilder = State OperationB ()

data OperationB = OperationB
  { _operationTypeB        :: Either () OperationType
  , _operationTagsB        :: Maybe [Text]
  , _operationSummaryB     :: Maybe Text
  , _operationDescriptionB :: Maybe Text
  , _operationResponsesB   :: [Either ResponseErr Responses]
  } deriving (Eq, Show)

$(makeLenses ''OperationB)

configOperation :: OperationBuilder -> Either OperationErr Operation
configOperation = convertO . flip execState emptyOperationB

convertO :: OperationB -> Either OperationErr Operation
convertO (OperationB _ (Just []) _ _ _)   = Left InvalidTags
convertO (OperationB _ _ (Just "") _ _)   = Left InvalidSummaryO
convertO (OperationB _ _ _ (Just "") _ )  = Left InvalidDescriptionO
convertO (OperationB (Left _) _ _ _ _)    = Left InvalidType
convertO (OperationB _ _ _ _ [])          = Left NoResponses
convertO (OperationB (Right t) ts s d rs) | not . noEmptyTxtsMaybe $ ts = Left InvalidTags
                                          | apIfRight False ((/=1) . length . filter isDefault) rs = Left MoreThanOneDefault
                                          | otherwise = foldBuilder InvalidResponse (Operation t ts s d) rs

typeOperation :: OperationType -> OperationBuilder
typeOperation t = modify $ operationTypeB .~ pure t

tagOperation :: Text -> OperationBuilder
tagOperation o = modify $ operationTagsB %~ pure . maybe [] (o:)

summaryOperation :: Text -> OperationBuilder
summaryOperation s = modify $ operationSummaryB ?~ s

descriptionOperation :: Text -> OperationBuilder
descriptionOperation d = modify $ operationDescriptionB ?~ d

defaultResponseOperation :: Either ResponseErr Response -> OperationBuilder
defaultResponseOperation (Right e) = modify $
  operationResponsesB %~ (pure (Default e):)
defaultResponseOperation (Left e)  = modify $
  operationResponsesB %~ (Left e:)

statusResponseOperation :: Text -> Either ResponseErr Response -> OperationBuilder
statusResponseOperation "" _ = modify $
  operationResponsesB %~ (Left InvalidHttpStatus:) -- better test needed
statusResponseOperation status (Right e) = modify $
  operationResponsesB %~ (Right (Status status e):)
statusResponseOperation _ (Left e) = modify $ operationResponsesB %~ (Left e:)


emptyOperationB :: OperationB
emptyOperationB = OperationB (Left ()) Nothing Nothing Nothing []
