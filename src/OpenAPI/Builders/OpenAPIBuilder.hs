{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.OpenAPIBuilder
  ( config
  , infoOpenAPI
  , pathOpenAPI
  , serverOpenAPI
  , securityOpenAPI
  , OpenAPIBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Bifunctor (first)
import Data.Text (Text)
import Lens.Micro ((%~), (.~), (^.))
import Lens.Micro.TH
import OpenAPI.Errors
import OpenAPI.Types
import OpenAPI.Utils

type OpenAPIBuilder = State OpenAPIB ()

data OpenAPIB = OpenAPIB
  { openAPIB       :: Text
  , _openInfoB     :: Either InfoErr Info
  , _openSecurityB :: [Either SecReqErr SecReq]
  , _openPathsB    :: [Either PathErr Path]
  , _openServersB  :: [Either ServerErr Server]
  } deriving (Eq, Show)

$(makeLenses ''OpenAPIB)

config :: State OpenAPIB () -> Either OpenAPIErr OpenAPI
config = convertS . flip execState emptyOpenAPIB

convertS :: OpenAPIB -> Either OpenAPIErr OpenAPI
convertS (OpenAPIB _ (Left e) _ _ _)  = Left . InvalidInfo $ e
convertS (OpenAPIB _ _ _ [] _)        = Left NoPaths
convertS (OpenAPIB v (Right i) sec p s) = do
  securityS <- first InvalidSecurity . sequence $ sec
  serverS   <- first InvalidServer   . sequence $ s
  pathS     <- foldBuilder InvalidPath (\z -> OpenAPI v i securityS z serverS) p
  noRepRecord (^.openPaths) (^.pathName) RepPaths pathS

infoOpenAPI :: Either InfoErr Info -> OpenAPIBuilder
infoOpenAPI i = modify $ openInfoB .~ i

pathOpenAPI :: Either PathErr Path -> OpenAPIBuilder
pathOpenAPI p = modify $ openPathsB %~ (p:)

serverOpenAPI :: Either ServerErr Server -> OpenAPIBuilder
serverOpenAPI s = modify $ openServersB %~ (s:)

securityOpenAPI :: Either SecReqErr SecReq -> OpenAPIBuilder
securityOpenAPI s = modify $ openSecurityB %~ (s:)

emptyOpenAPIB :: OpenAPIB
emptyOpenAPIB = OpenAPIB "3.0.2" (Left NoInfo) [] [] []
