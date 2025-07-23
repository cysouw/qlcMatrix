# qlcMatrix, version 0.9.9

This is an update after a notice from CRAN about errors in hyperlinks

## Changes

- revised ngram computations
- bugfixes, including formatting of hyperlinks

## Test environments
* local macOS Sequoia 15.4.1, R version 4.5.1
* CRAN win-builder https://win-builder.r-project.org, R version 4.4.0
* rhub::rhub_check() workflow for Linux, old macOS and Windows

## Downstream Dependencies
checked via revdepcheck::revdep_check() on local macOS Sequoia 15.4.1
No problems found

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 3 NOTEs

## Rcheck Notes

- A link to stackoverflow results in Status: 403 (forbidden), but the link works
- unable to verify current time
- Example with CPU (user + system) or elapsed time > 5s (splitWordlist, elapsed 5.206)
