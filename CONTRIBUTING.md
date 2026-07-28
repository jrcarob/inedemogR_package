# Contributing to inedemogR

Thanks for your interest in contributing!

## Bug reports and feature requests

Please file an issue describing the problem or request, including a minimal
reproducible example where relevant (see the
[reprex](https://reprex.tidyverse.org/) package).

## Pull requests

1. Fork the repo and create a branch from `main`.
2. Run `devtools::document()` after adding or editing roxygen2 comments.
3. Run `devtools::test()` and make sure all tests pass; add tests for new
   functionality.
4. Run `devtools::check()` before opening the PR.
5. Describe your change and reference any related issue in the PR
   description.

## Code style

Follow the [tidyverse style guide](https://style.tidyverse.org/). Function
names use `snake_case`; exported functions are documented with roxygen2.

## Code of Conduct

Please note that this project is released with a Contributor Code of
Conduct. By participating you agree to abide by its terms.
