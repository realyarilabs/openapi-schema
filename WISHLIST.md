# Things to improve, add or completely rewrite


## Global

 - Remove all warnings
 - Remove Prelude (maybe use Protolude instead?)
 - Add a Makefile
 - Add documentation


## Builders

 - The `Paths`s and `Operation`s could be in a `Set`


## Good first issues:

 - Optional lists can be `[a]` instead of `Maybe [a]`
 - Strip all `Text`s when verifying
 - Clean control flow. Use more monadic code.


## CircleCI
 - Remove hard-coded version number [i.e. by using a CircleCI environmental variable]
 - Add support for CodeCov
