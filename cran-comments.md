# qlcMatrix, version 0.9.8

This is a resubmission after receiving comments from CRAN

New submission
because package was archived on CRAN
Archived on 2023-11-29 because issues were not corrected in time.

## Test environments
* local macOS X 14.2.1, R version 4.3.2
* CRAN win-builder https://win-builder.r-project.org, R version 4.4.0
* rhub workflow for Linux, old macOS and Windows

## R CMD check results
There were no ERRORs or WARNINGs. 

One NOTEs remaining:

- a NOTE about the archived version as mentioned at the start. This submission is supposed to fix those issues.

## Downstream Dependencies
checked via revdepcheck::revdep_check() on local macOS X 14.2.1
No problems found

Comments to package review:

- corrected DESCRIPTION
- There are indeed literature references in the Rd-files of this package, but they are very specific to individual functions. It does not seem fitting to elevate them to the main DESCRIPTION file.
- removed most "\dontrun" because with more modern computers many calculations dropped below 5 sec.
- changed the few remaining examples of "\dontrun" into "\donttest"
- removed unexecutable code (forgotten leftover from earlier attempts)
- changed resetting of user's options to a more resilient approach: oldpar<-par("mfrow")...par(mfrow=oldpar)
