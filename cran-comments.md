## Submission

This is a new submission.

## Test environments

* local Ubuntu 24.04, R 4.6.1 (`R CMD check --as-cran`)
* local Ubuntu 24.04, R 4.6.1 with `_R_CHECK_DEPENDS_ONLY_=true`
  (only hard dependencies installed)
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release,
  oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Achmad Syahrul Choir <madsyair@stis.ac.id>'
  New submission

## Notes for the reviewers

* All model families use packages in `Suggests`; each is loaded with
  `requireNamespace()` when the model is used, and the examples, tests and
  vignette run conditionally, so the package passes without them.
* Examples run in under one second each; the test suite runs in about
  12 seconds and uses at most two cores.
* The example data (`idn_nowcast`) come from FRED; the script that builds
  them is in `data-raw/` of the source repository.
