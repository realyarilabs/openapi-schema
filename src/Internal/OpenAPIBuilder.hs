{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Internal.OpenAPIBuilder
  ( config
  , versionOpenAPI
  , infoOpenAPI
  , pathOpenAPI
  ) where

import           Control.Monad.State (State, execState, modify)
import           Data.Text           (Text)
import           Internal.Errors     (InfoErr (..), OpenAPIErr (..), PathErr)
import           Internal.Types      (Info, OpenAPI (..), Path)
import           Lens.Micro          ((%~), (.~))
import           Lens.Micro.TH


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
convertS (OpenAPIB v (Right i) p) =
  let paths = [ x | Left x <- p]
  in if null paths then
       Right (OpenAPI v i [ x | Right x <- p ])
     else
       Left . InvalidPath . head $ paths


versionOpenAPI :: Text -> OpenAPIBuilder
versionOpenAPI o = modify $ openAPIB .~ o

infoOpenAPI :: Either InfoErr Info -> OpenAPIBuilder
infoOpenAPI i = modify $ openInfoB .~ i

pathOpenAPI :: Either PathErr Path -> OpenAPIBuilder
pathOpenAPI p = modify $ openPathsB %~ (p:)


emptyOpenAPIB :: OpenAPIB
emptyOpenAPIB = OpenAPIB "3.0.0" (Left NoInfo) []
