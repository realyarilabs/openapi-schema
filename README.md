# OpenAPI

[![CircleCI](https://circleci.com/gh/herulume/OAS/tree/master.svg?style=svg)](https://circleci.com/gh/herulume/OAS/tree/master)

## About OpenAPI

The OAS Haskell Library implements an eDSL used to generate valid OpenAPI 3 configurations.

[Haddock Documentation](https://herulume.github.io/OAS)

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.Text
import Data.Text.Lazy.IO as I
import OpenAPI

main :: IO ()
main = dumpConfig "config.json" spec

spec :: Either OpenAPIErr OpenAPI
spec = config $ do
    info $ do
        versionInfo "Type 40"
        titleInfo "Foo's API"
        descriptionInfo "A cute API"
        tosInfo "http://foosocool"
        license $ do
            nameLicense "A cool license"
            urlLicense "http://veryURL"
        contact $ do
            nameContact "B"
            emailContact "b@foo"

    path $ do
        namePath "/tardis"
        summaryPath "Al requests for this endpoint travel in time"
        descriptionPath "Endpoint for TARIS"
        operation $ do
            typeOperation POST
            descriptionOperation "Update the tardis, type 40 is not enough"
            defaultRes $
                descriptionResponse "all ok"
            statusRes "404" $
                descriptionResponse "not found"
```

## Contributing

Please read [CONTRIBUTING](CONTRIBUTING.md) and [CODE_OF_CONDUCT](CODE_OF_CONDUCT.md) for details on our code of conduct and the process for submitting pull requests to us.
If you're out of ideas, but you still want to contribute, you can read our [WISHLIST](WISHLIST.md) for a list of things we can still improve on.


## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.txt) file for details.
