{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.SchemaBuilder
  ( configSchema
  , nullableSchema
  , discriminatorSchema
  , readOnlySchema
  , writeOnlySchema
  , xmlSchema
  , externalDocsSchema
  , exampleSchema
  , deprecatedSchema
  , SchemaBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((.~), (?~))
import Lens.Micro.TH
import OpenAPI.Errors
import OpenAPI.Types
import OpenAPI.Utils


type SchemaBuilder = State SchemaB ()

data SchemaB = SchemaB
  { _schemaNullableB      :: Bool
  , _schemaDiscriminatorB :: Either DiscriminatorErr Discriminator
  , _schemaReadOnlyB      :: Bool
  , _schemaWriteOnlyB     :: Bool
  , _schemaXmlB           :: Either XmlErr Xml
  , _schemaExternalDocsB  :: Either ExternalDocsErr ExternalDocs
  , _schemaExampleB       :: Maybe Text
  , _schemaDeprecatedB    :: Bool
  } deriving (Eq, Show)

$(makeLenses ''SchemaB)

configSchema :: SchemaBuilder -> Either SchemaErr Schema
configSchema = convertC . flip execState emptySchemaB

convertC :: SchemaB -> Either SchemaErr Schema
convertC SchemaB{_schemaDiscriminatorB = (Left e)} = Left . DiscriminatorError $ e
convertC SchemaB{_schemaXmlB = (Left e)} = Left . XmlError $ e
convertC SchemaB{_schemaExternalDocsB = (Left e)} = Left . DocsError $ e
convertC (SchemaB n (Right d) r w (Right x) (Right ed) e dep) | emptyTxtMaybe e = Left InvalidExampleSchema
                                                              | r && w = Left WriteAndReadOnly
                                                              | otherwise = pure $ Schema n d r w x ed e dep

nullableSchema :: SchemaBuilder
nullableSchema = modify $ schemaNullableB .~ True

discriminatorSchema :: Either DiscriminatorErr Discriminator -> SchemaBuilder
discriminatorSchema d = modify $ schemaDiscriminatorB .~ d

readOnlySchema :: SchemaBuilder
readOnlySchema = modify $ schemaReadOnlyB .~ True

writeOnlySchema :: SchemaBuilder
writeOnlySchema = modify $ schemaWriteOnlyB .~ True

xmlSchema :: Either XmlErr Xml -> SchemaBuilder
xmlSchema xml = modify $ schemaXmlB .~ xml

externalDocsSchema :: Either ExternalDocsErr ExternalDocs -> SchemaBuilder
externalDocsSchema doc = modify $ schemaExternalDocsB .~ doc

exampleSchema :: Text -> SchemaBuilder
exampleSchema e = modify $ schemaExampleB ?~ e

deprecatedSchema :: SchemaBuilder
deprecatedSchema = modify $ schemaDeprecatedB .~ True

emptySchemaB :: SchemaB
emptySchemaB = SchemaB False (Left InvalidNameDiscriminator) False False (Left InvalidNameXml) (Left InvalidURLDocs) Nothing False
