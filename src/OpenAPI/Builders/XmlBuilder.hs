{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.XmlBuilder
  ( configXml
  , nameXml
  , nameSpaceXml
  , prefixXml
  , attributeXml
  , wrappedXml
  , XmlBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((.~))
import Lens.Micro.TH
import OpenAPI.Errors (XmlErr (..))
import OpenAPI.Types (Xml (..))
import OpenAPI.Utils


type XmlBuilder = State XmlB ()

data XmlB = XmlB
  { _xmlNameB      :: Text
  , _xmlNameSpaceB :: Text
  , _xmlPrefixB    :: Text
  , _xmlAttributeB :: Bool
  , _xmlWrappedB   :: Bool
  } deriving (Eq, Show)

$(makeLenses ''XmlB)

configXml :: XmlBuilder -> Either XmlErr Xml
configXml = convertC . flip execState emptyXmlB

convertC :: XmlB -> Either XmlErr Xml
convertC (XmlB n ns p a w ) | emptyTxt n = Left InvalidNameXml
                            | emptyTxt ns = Left InvalidNameSpaceXml
                            | emptyTxt p = Left InvalidPrefixXml
                            | otherwise = pure $ Xml n ns p a w

nameXml :: Text -> XmlBuilder
nameXml n = modify $ xmlNameB .~ n

nameSpaceXml :: Text -> XmlBuilder
nameSpaceXml ns = modify $ xmlNameSpaceB .~ ns

prefixXml :: Text -> XmlBuilder
prefixXml p = modify $ xmlPrefixB .~ p

attributeXml :: XmlBuilder
attributeXml = modify $ xmlAttributeB .~ True

wrappedXml :: XmlBuilder
wrappedXml = modify $ xmlWrappedB .~ True

emptyXmlB :: XmlB
emptyXmlB = XmlB "" "" "" False False
