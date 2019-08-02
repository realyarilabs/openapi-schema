module Builders
    (
      module Builders.ContactBuilder
    , module Builders.InfoBuilder
    , module Builders.LicenseBuilder
    , module Builders.PathBuilder
    , module Builders.OpenAPIBuilder
    , module Builders.OperationBuilder
    , module Builders.ResponseBuilder
    , info
    , license
    , contact
    , path
    , operation
    , defaultRes
    , statusRes
    ) where

import Builders.ContactBuilder
import Builders.InfoBuilder
import Builders.LicenseBuilder
import Builders.OpenAPIBuilder
import Builders.OperationBuilder
import Builders.PathBuilder
import Builders.ResponseBuilder
import Data.Text (Text)


info :: InfoBuilder -> OpenAPIBuilder
info = infoOpenAPI . configInfo

license :: LicenseBuilder -> InfoBuilder
license = licenseInfo . configLicense

contact :: ContactBuilder -> InfoBuilder
contact = contactInfo . configContact

path :: PathBuilder -> OpenAPIBuilder
path = pathOpenAPI . configPath

operation :: OperationBuilder -> PathBuilder
operation = operationPath . configOperation

defaultRes :: ResponseBuilder -> OperationBuilder
defaultRes = defaultResponseOperation . configResponse

statusRes :: Text -> ResponseBuilder -> OperationBuilder
statusRes s = statusResponseOperation s . configResponse
