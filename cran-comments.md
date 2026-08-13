## Test environments

* Windows 11 x64, R 4.6.0 (2026-04-24 ucrt) -- local, `R CMD check --as-cran`
* win-builder, R-release -- `devtools::check_win_release()`
* win-builder, R-devel -- `devtools::check_win_devel()`
* win-builder, R-oldrelease -- `devtools::check_win_oldrelease()`
* macOS (tested by the maintainer)
* Posit Cloud / Linux (tested by the maintainer)
* Windows Server (tested by the maintainer)

## R CMD check results

0 errors, 0 warnings, 1 NOTE, on every environment listed above.

The remaining NOTE is `checking CRAN incoming feasibility`, and it has two
parts, both expected:

* *New submission.* This is the package's first release.
* *Possibly misspelled words in DESCRIPTION: Kaplan.* "Kaplan" is a surname:
  the DESCRIPTION describes the Kaplan-Meier estimator, after Edward L. Kaplan
  and Paul Meier. The spelling is correct.

`R CMD check` reports no unstated dependencies in the tests or the vignette.

## Accepted input types

`ggstratify()` takes one thing: a data frame -- `tibble` and `data.table`
included -- or a matrix, already read into the user's session. A file path is
deliberately not accepted, and the application has no file-upload control.
Reading a file would set every column's type by guess, and the package would
then describe the data on those guesses; the documentation asks the user to
read the file, check the types and pass the resulting object instead.

That claim is tested rather than asserted: `tests/testthat/` covers a
`data.frame`, a `data.table`, a `tibble`, a class-subclassed data frame
standing in for a grouped `tibble`, and a matrix, and asserts that a path is
rejected and that no file control appears anywhere in the UI. Each accepted
input is coerced to a fresh, plain `data.table`, so a subclass never reaches
the code generator, a grouping is never mistaken for a stratification, and
writing into the result by reference cannot reach back into the caller's
object.

`tibble` is used only by that one test and is declared in `Suggests`, guarded
by `skip_if_not_installed()`; the package itself does not depend on it.

## Test suite

`devtools::test()` on Windows 11 x64 with R 4.6.0: 620 passing, 0 failures,
0 warnings, 0 skips.

The preview subsampling caps are one million rows (scatter and line plots) and
ten million rows (the plot types that summarise their rows before drawing).
The tests exercise the sampling rules through an injected threshold rather
than by allocating a table of that size, so the suite stays within a check
machine's memory.
