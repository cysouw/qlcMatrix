## Test environments
* local OS X 10.13.4 install, R 3.4.3
* CRAN win-builder via devtools

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTES:

* checking data for non-ASCII characters ... NOTE
  Note: found 76102 marked UTF-8 strings
  
  ==> The data includes real data, which includes non-ASCII, but UTF-8 text
  
* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
  
  ==> I couldn't get rid of this one, probably something wrong with my .Rprofile?

On win-builder there were two examples marked as long:

** running examples for arch 'i386' ... OK
Examples with CPU or elapsed time > 5s
             user system elapsed
sim.wordlist 9.16   0.28    9.44
** running examples for arch 'x64' ... OK
Examples with CPU or elapsed time > 5s
             user system elapsed
sim.wordlist 7.68    0.1    7.79

  ==> I can start marking all examples as 'dontrun', but the point of the whole
  package is the processing of large datasets, so I would like to keep a few 
  reasonably large examples in, if possible.

## Downstream dependencies
There are no downstream dependencies (yet)