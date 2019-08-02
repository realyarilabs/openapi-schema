{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Builders.OpenAPIBuilder
  ( config
  , versionOpenAPI
  , infoOpenAPI
  , pathOpenAPI
  , OpenAPIBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Either (isLeft, lefts, rights)
import Data.Text (Text)
import Errors (InfoErr (..), OpenAPIErr (..), PathErr)
import Lens.Micro ((%~), (.~))
import Lens.Micro.TH
import Types (Info, OpenAPI (..), Path)
import Utils (foldBuilder)

type OpenAPIBuilder = State OpenAPIB ()

data OpenAPIB = OpenAPIB
  { _openAPIB   :: Text
  , _openInfoB  :: Either InfoErr Info
  , _openPathsB :: [Either PathErr Path]
  } deriving (Eq, Show)

$(makeLenses ''OpenAPIB)

config :: State OpenAPIB () -> Either OpenAPIErr OpenAPI
config = convertS . flip execState emptyOpenAPIB

convertS :: OpenAPIB -> Either OpenAPIErr OpenAPI
convertS (OpenAPIB "" _ _)        = Left InvalidLicenseV
convertS (OpenAPIB _ (Left e) _)  = Left . InvalidInfo $ e
convertS (OpenAPIB _ _ [])        = Left NoPaths
convertS (OpenAPIB v (Right i) p) = foldBuilder p InvalidPath (OpenAPI v i)


versionOpenAPI :: Text -> OpenAPIBuilder
versionOpenAPI o = modify $ openAPIB .~ o

infoOpenAPI :: Either InfoErr Info -> OpenAPIBuilder
infoOpenAPI i = modify $ openInfoB .~ i

pathOpenAPI :: Either PathErr Path -> OpenAPIBuilder
pathOpenAPI p = modify $ openPathsB %~ (p:)


emptyOpenAPIB :: OpenAPIB
emptyOpenAPIB = OpenAPIB "3.0.0" (Left NoInfo) []
