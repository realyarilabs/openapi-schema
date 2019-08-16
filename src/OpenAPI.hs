module OpenAPI
  (
    module OpenAPI.Builders
  , module OpenAPI.Types
  , module OpenAPI.Errors
  , dumpConfig
  ) where

import           Data.Aeson.Text
import qualified Data.Text.Lazy.IO as LI
import           OpenAPI.Builders
import           OpenAPI.Errors
import           OpenAPI.Types


dumpConfig :: FilePath -> Either OpenAPIErr OpenAPI -> IO ()
dumpConfig f = either print (LI.writeFile f . encodeToLazyText)
