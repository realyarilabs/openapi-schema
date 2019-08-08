{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.OpenAPIBuilder
  ( config
  , infoOpenAPI
  , pathOpenAPI
  , OpenAPIBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Either (isLeft, lefts, rights)
import Data.Text (Text)
import Lens.Micro ((%~), (.~), (^.))
import Lens.Micro.TH
import OpenAPI.Errors (InfoErr (..), OpenAPIErr (..), PathErr)
import OpenAPI.Types
import OpenAPI.Utils (allDifferent, cond, foldBuilder, noRepRecord)

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
convertS (OpenAPIB _ (Left e) _)  = Left . InvalidInfo $ e
convertS (OpenAPIB _ _ [])        = Left NoPaths
convertS (OpenAPIB v (Right i) p) = either Left (noRepRecord (^.openPaths) (^.pathName) RepPaths) . foldBuilder InvalidPath (OpenAPI v i) $ p


infoOpenAPI :: Either InfoErr Info -> OpenAPIBuilder
infoOpenAPI i = modify $ openInfoB .~ i

pathOpenAPI :: Either PathErr Path -> OpenAPIBuilder
pathOpenAPI p = modify $ openPathsB %~ (p:)


emptyOpenAPIB :: OpenAPIB
emptyOpenAPIB = OpenAPIB "3.0.2" (Left NoInfo) []
