## Test environments
* local OS X 10.11 install, R 3.2.1
* CRAN win-builder via devtools

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTES:

* checking data for non-ASCII characters ... NOTE
  Note: found 76102 marked UTF-8 strings
  
  ==> The data includes real data, which includes non-ASCII, but UTF-8 text
  
* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
  
  ==> I couldn't get rid of this one, probably something wrong with my .R

## Downstream dependencies
There are no downstream dependencies (yet)