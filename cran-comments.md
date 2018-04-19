## Test environments
* local macOS X 10.13.4, R version 3.4.3
* CRAN win-builder via devtools, R version 3.5.0 RC
* linux Ubuntu 14.04.5 LTS via travis-ci, R version 3.4.4

## R CMD check results
There were no ERRORs, NOTEs, or WARNINGs. 

## Downstream Dependencies
checked via devtools::revdep_check() on local macOS X 10.13.4
One error with package "DCD" because of other dependencies: packages required but not available: ‘Rdpack’ ‘ROCR’ ‘WGCNA’
