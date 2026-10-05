## Update: inedemogR 0.2.0

This update corrects numerical errors in 0.1.0 that were identified during
peer review of the accompanying software article (SoftwareX), so users of
the CRAN version obtain correct results. Main changes (see NEWS.md):

* Life tables now follow the Human Mortality Database Methods Protocol V6:
  the Andreev-Kingkade (2015) infant formulas are implemented correctly,
  old-age mortality uses the Kannisto model fitted by Poisson maximum
  likelihood, and the Lexis-triangle exposure correction is fixed.
* Corrected European Standard Population 2013 weights, abridged life
  tables, the Pollard decomposition, and fertility indicators (which now
  follow the conventions of the Spanish statistics institute, INE).
* Missing data are no longer turned into zero mortality, and the
  validation functions now fail on invalid input.

Some return values change (documented in NEWS.md under "Breaking
changes"). The package has no reverse dependencies on CRAN.

## Files written by the package

* No function writes to the user's home filespace by default.
* The retrieval functions' optional cache (`use_cache = TRUE`) now uses a
  per-session directory under `tempdir()` when `cache_dir = NULL` (in
  0.1.0, `cache_dir = NULL` silently disabled caching). A persistent cache
  is written only to a directory the user supplies explicitly.
* `update_ine_data()` still requires an explicit `data_dir`.

## R CMD check results

0 errors | 0 warnings | 1 note

* The NOTE ("checking for future file timestamps ... unable to verify
  current time") is an environment-dependent false positive from the
  local network time check.

## Notes for reviewers

* Functions that retrieve data from the INE API or the 'mapSpain' boundary
  service need network access; their examples are wrapped in `\dontrun{}`
  with a comment stating why. All other exported functions have runnable
  examples on small synthetic data.
* An integration test that downloads live INE data is skipped on CRAN
  (`skip_on_cran()`) and runs only when the environment variable
  `INEDEMOGR_LIVE_TESTS=true` is set.
* The vignette is a static LaTeX document (`R.rsp::tex`); building it makes
  no network calls.
