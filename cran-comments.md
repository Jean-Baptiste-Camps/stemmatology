
This is a resubmission to fix CRAN requests.

## Test environments
* local Ubuntu 16.04 install, R 3.4.4
* Ubuntu 14.04 (on travis-ci), R 3.5
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

## Examples and \dontrun{}

Part of the `\dontrun{}` in the examples were modified to `\donttest{}`, reduced or deleted, but some examples still use them, because functions like `PCC`, `PCC.Exploratory`, …, have user-interactions. These can't be run by R CMD check.
