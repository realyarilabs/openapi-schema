{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.MediaTypeBuilder
  ( configMediaType
  , exampleMediaType
  , MediaTypeBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((.~))
import Lens.Micro.TH
import OpenAPI.Errors (MediaTypeErr (..))
import OpenAPI.Types (MediaType (..))
import OpenAPI.Utils


type MediaTypeBuilder = State MediaTypeB ()

data MediaTypeB = MediaTypeB
  { _mediaTypeExampleB :: Text
  } deriving (Eq, Show)

$(makeLenses ''MediaTypeB)

configMediaType :: MediaTypeBuilder -> Either MediaTypeErr MediaType
configMediaType = convertC . flip execState emptyMediaTypeB

convertC :: MediaTypeB -> Either MediaTypeErr MediaType
convertC (MediaTypeB e) | emptyTxt e = Left InvalidExampleMediaType
                        | otherwise = pure . MediaType $ e

exampleMediaType :: Text -> MediaTypeBuilder
exampleMediaType e = modify $ mediaTypeExampleB .~ e


emptyMediaTypeB :: MediaTypeB
emptyMediaTypeB = MediaTypeB ""
