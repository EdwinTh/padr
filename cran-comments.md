Submissing patch release

As requested by Kurt Hornik, the behavior of sample() will be changed from R 3.6 onwards. Making some unit tests fail. The sampled data are replaced by hard coded values so this should be evaded now.

################################################

# padr v0.4.2

## Test environments
* local OS X install, R 3.5
* ubuntu 14.04.5 (on travis-ci), R 3.5
* win-builder

## R CMD check results

All tests passed without Errors, Warnings, or Notes on all platforms (including "checking for unstated dependencies in 'tests'").


There is one reverse dependency timetk. Since this is patch release the maintainers are not informed, changes cannot break dependencies.