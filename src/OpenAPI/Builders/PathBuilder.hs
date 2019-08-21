{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.PathBuilder
  ( configPath
  , namePath
  , summaryPath
  , descriptionPath
  , operationPath
  , refPath
  , serverPath
  , PathBuilder
  ) where

import           Control.Monad.State (State, execState, modify)
import           Data.Bifunctor (first)
import           Data.Text (Text)
import qualified Data.Text as T
import           Lens.Micro ((%~), (.~), (?~), (^.))
import           Lens.Micro.TH
import           OpenAPI.Errors
import           OpenAPI.Types
import           OpenAPI.Utils

type PathBuilder = State PathB ()

data PathB = PathB
  { _pathNameB        :: Text
  , _pathRefB         :: Maybe Text
  , _pathSummaryB     :: Maybe Text
  , _pathDescriptionB :: Maybe Text
  , _pathServersB     :: [Either ServerErr Server]
  , _pathOperationsB  :: [Either OperationErr Operation]
  } deriving (Eq, Show)

$(makeLenses ''PathB)

configPath :: PathBuilder -> Either PathErr Path
configPath = convertP . flip execState emptyPathB

convertP :: PathB -> Either PathErr Path
convertP (PathB _ _ _ _ _ []) = Left NoOperations
convertP (PathB n r s d ss o) | emptyTxt n = Left InvalidNameP
                              | emptyTxtMaybe s = Left InvalidSummaryP
                              | emptyTxtMaybe d = Left InvalidDescriptionP
                              | emptyTxtMaybe r = Left InvalidReferenceP
                              | otherwise = case T.head n of
                                  '/' -> do
                                           servers <- first InvalidServerP . sequence $ ss
                                           p <- foldBuilder InvalidOperation (Path n r s d servers) o
                                           noRepRecord (^.pathOperations) (^.operationType) RepResponses p
                                  _   -> Left InvalidNameP


namePath :: Text -> PathBuilder
namePath n = modify $ pathNameB .~ n

refPath :: Text -> PathBuilder
refPath r = modify $ pathRefB ?~ r

summaryPath :: Text -> PathBuilder
summaryPath s = modify $ pathSummaryB ?~ s

descriptionPath :: Text -> PathBuilder
descriptionPath d = modify $ pathDescriptionB ?~ d

operationPath :: Either OperationErr Operation -> PathBuilder
operationPath o = modify $ pathOperationsB %~ (o:)

serverPath :: Either ServerErr Server -> PathBuilder
serverPath s = modify $ pathServersB %~ (s:)

emptyPathB :: PathB
emptyPathB = PathB "" Nothing Nothing Nothing [] []
