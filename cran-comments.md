# qlcMatrix, version 0.9.8

This is a resubmission after receiving comments from CRAN

New submission because package was archived on CRAN
Archived on 2023-11-29 because issues were not corrected in time.

## Test environments
* local macOS X 14.4.1, R version 4.3.0
* CRAN win-builder https://win-builder.r-project.org, R version 4.4.0
* rhub::rhub_check() workflow for Linux, old macOS and Windows

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream Dependencies
checked via revdepcheck::revdep_check() on local macOS X 14.2.1
No problems found

## Actions taken after package review:

- I have corrected the DESCRIPTION-file
- There are indeed literature references in the Rd-files of this package, but they are very specific to individual functions. It does not seem fitting to elevate them to the main DESCRIPTION file.
- I removed most "\dontrun" because with more modern computers many calculations dropped below 5 sec.
- I changed most remaining examples of "\dontrun" into "\donttest". 
- Only real examples of errors remain as "\dontrun": either examples of limits to the computations, or a few graphics that raise errors on some platforms because of Unicode non-compliance.
- there are two remaining example sections with slightly over 5 seconds runtime on my computer
- I removed unexecutable code (forgotten leftover from earlier attempts)
- I changed resetting of user's options to a more resilient approach: oldpar<-par("mfrow")...par(mfrow=oldpar)
