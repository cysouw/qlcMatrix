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

### dontrun/donttest

- I removed many "\dontrun" because with more modern computers many calculations dropped below 5 sec.
- I changed most remaining examples of "\dontrun" into "\donttest". 
- Only real examples of errors remain as "\dontrun": either examples of limits to the computations, or a few graphics that raise errors on some platforms because of Unicode non-compliance.
- On the CRAN-devel-builder it took a bit more time, which resulted in automatic rejection. To alleviate this, I went back and encapsulated many more examples in "\donttest"
- However, as soon as I exclude some examples, examples of other functions start to report longer elapsed time. Very strange. Unfortunately, I had to exclude way more examples than I wanted to get inside the allowed elapsed time.

### minor corrections:

- I removed unexecutable code (forgotten leftover from earlier attempts)
- I changed resetting of user's options to a more resilient approach: oldpar<-par("mfrow")...par(mfrow=oldpar)
- I have corrected the DESCRIPTION-file
- There are indeed literature references in the Rd-files of this package, but they are very specific to individual functions. It does not seem fitting to elevate them to the main DESCRIPTION file.
