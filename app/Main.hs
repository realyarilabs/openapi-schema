{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson.Text

import           Internal.Builders
import           Internal.Errors
import           Internal.Types

import           Data.Text.Lazy.IO as I

main :: IO ()
main =  either print (I.writeFile "config.json" . encodeToLazyText) spec

spec :: Either OpenAPIErr OpenAPI
spec = config $ do
    -- version
    versionOpenAPI "3.0.2"
    -- info
    infoOpenAPI $
        configInfo $ do
            titleInfo "yay"
            descriptionInfo "asdfsgd"
            tosInfo "WTFYW"
            licenseInfo $
                configLicense $
                    nameLicense "BSD"
            versionInfo "3.2.1"
            contactInfo $
                configContact $ do
                    nameContact "Eduardo"
                    emailContact "test@herulu.me"
    pathOpenAPI $
        configPath $ do
             namePath "/coisas"
             summaryPath "Pois"
             descriptionPath "faz coisas"
             operationPath $
                configOperation $ do
                    typeOperation POST
                    descriptionOperation "get coisas"
                    defaultResponseOperation $
                        configResponse $
                            descriptionResponse "all ok"
                    statusResponseOperation "200" $
                        configResponse $
                            descriptionResponse "all ok"
                    statusResponseOperation "404" $
                        configResponse $
                            descriptionResponse ""
