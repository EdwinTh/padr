# padr v0.2.1

Fixed the NOTE (as requested by Kurt Hornik):

File 'padr/libs/i386/padr.dll':
  Found no calls to: 'R_registerRoutines', 'R_useDynamicSymbols'
File 'padr/libs/x64/padr.dll':
  Found no calls to: 'R_registerRoutines', 'R_useDynamicSymbols'

By appliying Kevin Ushey's functions to register the routines.

https://github.com/kevinushey/sourcetools/blob/master/R/register.R

No more NOTE on win-builder.

########################################################################################

## Test environments
* local OS X install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

On win-builder only there were two NOTES

1) the standard not:
* checking CRAN incoming feasibility

2) a new note that I never saw before:
* checking compiled code ... NOTE
File 'padr/libs/i386/padr.dll':
  Found no calls to: 'R_registerRoutines', 'R_useDynamicSymbols'
File 'padr/libs/x64/padr.dll':
  Found no calls to: 'R_registerRoutines', 'R_useDynamicSymbols'

Which appears a known, and still open, issue at the Rcpp development team. https://github.com/RcppCore/Rcpp/issues/636. From my best judgement this issue is not blocking for the working of padr.


## Reverse dependencies

There are currently no downstream dependencies for this package