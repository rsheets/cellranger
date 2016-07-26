This is an update to resolve a test that required modification to work with testthat v1.0.x.

## Test environments
* local OS X install, R 3.3.0
* ubuntu 12.04 on travis-ci, R 3.3.1
* win-builder, devel and release 3.2.1

## R CMD check results

There were no ERRORs or WARNINGs. There is one NOTE: the standard one re: MIT license.

## Downstream dependencies

There are two: googlesheets and readODS. Both pass R CMD check.
