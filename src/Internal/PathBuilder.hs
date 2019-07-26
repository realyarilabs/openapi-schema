{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Internal.PathBuilder
  ( configPath
  , namePath
  , summaryPath
  , descriptionPath
  , operationPath
  ) where

import           Control.Monad.State (State, execState, modify)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Internal.Errors     (OperationErr, PathErr (..))
import           Internal.Types      (Operation, Path (..))
import           Lens.Micro          ((%~), (.~), (?~))
import           Lens.Micro.TH


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
  let operations = [ x | Left x <- o ]
  in case T.head n of
       '/' -> if null operations then
                Right $ Path n s d [ x | Right x <- o ]
              else
                Left . InvalidOperation . head $ operations
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
