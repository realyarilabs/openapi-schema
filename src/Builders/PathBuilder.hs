{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Builders.PathBuilder
  ( configPath
  , namePath
  , summaryPath
  , descriptionPath
  , operationPath
  , PathBuilder
  ) where

import           Control.Monad.State (State, execState, modify)
import           Data.Text (Text)
import qualified Data.Text as T
import           Errors (OperationErr, PathErr (..))
import           Lens.Micro ((%~), (.~), (?~), (^.))
import           Lens.Micro.TH
import           Types
import           Utils (allDifferent, cond, foldBuilder, noRepRecord)

type PathBuilder = State PathB ()

data PathB = PathB
  { _pathNameB        :: Text
  , _pathSummaryB     :: Maybe Text
  , _pathDescriptionB :: Maybe Text
  , _pathOperationsB  :: [Either OperationErr Operation]
  } deriving (Eq, Show)

$(makeLenses ''PathB)

configPath :: PathBuilder -> Either PathErr Path
configPath = convertP . flip execState emptyPathB

convertP :: PathB -> Either PathErr Path
convertP (PathB "" _ _ _)        = Left InvalidNameP
convertP (PathB _ (Just "") _ _) = Left InvalidSummaryP
convertP (PathB _ _ (Just "") _) = Left InvalidDescriptionP
convertP (PathB _ _ _ [])        = Left NoOperations
convertP (PathB n s d o)         =
  case T.head n of
    '/' ->  either Left (noRepRecord (^.pathOperations) (^.operationType) RepResponses) . foldBuilder InvalidOperation (Path n s d) $ o
    _   -> Left InvalidNameP


namePath :: Text -> PathBuilder
namePath n = modify $ pathNameB .~ n

summaryPath :: Text -> PathBuilder
summaryPath s = modify $ pathSummaryB ?~ s

descriptionPath :: Text -> PathBuilder
descriptionPath d = modify $ pathDescriptionB ?~ d

operationPath :: Either OperationErr Operation -> PathBuilder
operationPath o = modify $ pathOperationsB %~ (o:)


emptyPathB :: PathB
emptyPathB = PathB "" Nothing Nothing []
