{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.OpenAPIBuilder
  ( config
  , infoOpenAPI
  , pathOpenAPI
  , serverOpenAPI
  , OpenAPIBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Either
import Data.Text (Text)
import Lens.Micro ((%~), (.~), (^.))
import Lens.Micro.TH
import OpenAPI.Errors
import OpenAPI.Types
import OpenAPI.Utils

type OpenAPIBuilder = State OpenAPIB ()

data OpenAPIB = OpenAPIB
  { _openAPIB     :: Text
  , _openInfoB    :: Either InfoErr Info
  , _openPathsB   :: [Either PathErr Path]
  , _openServersB :: [Either ServerErr Server]
  } deriving (Eq, Show)

$(makeLenses ''OpenAPIB)

config :: State OpenAPIB () -> Either OpenAPIErr OpenAPI
config = convertS . flip execState emptyOpenAPIB

convertS :: OpenAPIB -> Either OpenAPIErr OpenAPI
convertS (OpenAPIB _ (Left e) _ _)  = Left . InvalidInfo $ e
convertS (OpenAPIB _ _ [] _)        = Left NoPaths
convertS (OpenAPIB v (Right i) p s) =
  let servers = sequence s
  in  either (Left . InvalidServer) build servers where
        build servers = checkRep . foldBuilder InvalidPath (\z -> OpenAPI v i z servers) $ p
        checkRep = either Left (noRepRecord (^.openPaths) (^.pathName) RepPaths)


infoOpenAPI :: Either InfoErr Info -> OpenAPIBuilder
infoOpenAPI i = modify $ openInfoB .~ i

pathOpenAPI :: Either PathErr Path -> OpenAPIBuilder
pathOpenAPI p = modify $ openPathsB %~ (p:)

serverOpenAPI :: Either ServerErr Server -> OpenAPIBuilder
serverOpenAPI s = modify $ openServersB %~ (s:)

emptyOpenAPIB :: OpenAPIB
emptyOpenAPIB = OpenAPIB "3.0.2" (Left NoInfo) [] []
