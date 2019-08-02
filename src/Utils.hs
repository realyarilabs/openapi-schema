module Utils where


import Control.Monad
import Data.Aeson (KeyValue, ToJSON, (.=))
import Data.Bifunctor (bimap)
import Data.Either (Either (..), either)
import Data.List (zipWith)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Traversable (sequence)

{-
   Some records have optional fields, this are represented by a `Maybe a`. When
   generating the JSON representation we do not want to have fields with the
   `Nothing` value, since this is transpiled to a `null`.
   This function removes all the `Nothing`s and generates the respective
   `Pair`s (Name, Value) to be converted JSON.

-}
pairMaybes :: (ToJSON a, KeyValue b) => [Maybe a] -> [Text] -> [b]
pairMaybes a b = join $ zipWith (\x y -> if isNothing x then [] else [y .= x]) a b



{-
    Some records fields depend on a list of other records that may have failed
    building. On such cases we want to propagate the error.
-}
foldBuilder :: [Either b a] -> (b -> c) -> ([a] -> d) -> Either c d
foldBuilder l err build = bimap err build . sequence $ l

