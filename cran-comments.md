# padr v0.4.2

Patch release as requested by Kurt Hornik. Removed `sample` from unit tests so all unit tests will pass with the new sampler introduced in R 3.6.0

## Test environments
* local OS X install, R 3.5.3
* Ubuntu 14.04.5 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

On win-builder only there was the standard NOTE
* checking CRAN incoming feasibility

There is one reverse dependency timetk. Its maintainer Matt Dancho has received a personal email and assured me the new version works with timetk.