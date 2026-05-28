## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Test environments

* local: <preencher: ex. Windows 11, R 4.5.0>
* win-builder (devel and release)
* R-hub (linux, windows, macos)

## Notes for submission

* Spark-dependent functionality (sparklyr / tbl_spark paths) is exercised
  only in optional examples wrapped in \donttest{} and is not required for
  the package to load or for the test suite to pass; all unit tests run on
  local data frames without a JVM.
* The DESCRIPTION Authors@R uses a placeholder maintainer email that must be
  replaced with a real, monitored address before submission.
