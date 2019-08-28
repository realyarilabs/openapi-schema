module OpenAPI.Builders
    (
      module OpenAPI.Builders.ContactBuilder
    , module OpenAPI.Builders.InfoBuilder
    , module OpenAPI.Builders.LicenseBuilder
    , module OpenAPI.Builders.PathBuilder
    , module OpenAPI.Builders.OpenAPIBuilder
    , module OpenAPI.Builders.OperationBuilder
    , module OpenAPI.Builders.ResponseBuilder
    , module OpenAPI.Builders.ServerVarBuilder
    , module OpenAPI.Builders.ServerBuilder
    , module OpenAPI.Builders.SecReqBuilder
    , module OpenAPI.Builders.ReferenceBuilder
    , module OpenAPI.Builders.ParameterBuilder
    , module OpenAPI.Builders.DiscriminatorBuilder
    , module OpenAPI.Builders.XmlBuilder
    , module OpenAPI.Builders.ExternalDocsBuilder
    , info
    , license
    , contact
    , path
    , operation
    , defaultRes
    , statusRes
    ) where

import Data.Text (Text)
import OpenAPI.Builders.ContactBuilder
import OpenAPI.Builders.DiscriminatorBuilder
import OpenAPI.Builders.ExternalDocsBuilder
import OpenAPI.Builders.InfoBuilder
import OpenAPI.Builders.LicenseBuilder
import OpenAPI.Builders.OpenAPIBuilder
import OpenAPI.Builders.OperationBuilder
import OpenAPI.Builders.ParameterBuilder
import OpenAPI.Builders.PathBuilder
import OpenAPI.Builders.ReferenceBuilder
import OpenAPI.Builders.ResponseBuilder
import OpenAPI.Builders.SecReqBuilder
import OpenAPI.Builders.ServerBuilder
import OpenAPI.Builders.ServerVarBuilder
import OpenAPI.Builders.XmlBuilder

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
