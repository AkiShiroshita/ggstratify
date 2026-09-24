## Submission

This is an update to ggstratify 0.0.1, on CRAN since 2026-09-02. Version 0.2.0
adds survey-weight support with design-based intervals from the 'survey'
package, a time-resolution method for deriving a variable from a date or a
date-time, confidence intervals on the Dot + Error figure, and a
missing-vs-observed method for deriving a variable. `NEWS.md` lists the
changes; there are no user-visible changes to the existing interface and no
deprecations.

0.0.1 was published three weeks ago, sooner than the one-to-two-month interval
the Repository Policy asks updates to keep to. If you would rather hold this
one until late October, please do; nothing here is urgent, and I will not send
another update before then.

## Test environments

* Windows 11 x64, R 4.6.0 (2026-04-24 ucrt) -- local, `R CMD check --as-cran`
* win-builder, R-release -- `devtools::check_win_release()` (2026-09-24)
* win-builder, R-devel -- `devtools::check_win_devel()` (2026-09-24)
* macOS builder, macOS Tahoe 26.6 (aarch64-apple-darwin23),
  R 4.6.1 Patched (2026-07-27 r90311) -- `devtools::check_mac_release()`
* R-hub v2 / Ubuntu 24.04.5 LTS (x86_64-pc-linux-gnu), R-devel (2026-09-23 r90586)
* R-hub v2 / macOS Sequoia 15.7.9, R-devel (2026-09-23 r90587)
* R-hub v2 / Windows Server 2022 x64 (x86_64-w64-mingw32), R-devel (2026-09-23 r90587 ucrt)

## R CMD check results

0 errors | 0 warnings | 0 notes

The local `--as-cran` run, the macOS builder run and all three R-hub v2
platforms finished with `Status: OK`. The "New submission" NOTE of 0.0.1 no
longer applies, and `checking CRAN incoming feasibility` is clean: the
DESCRIPTION spelling that was queried last time ("Kaplan", a surname) is no
longer flagged.

`R CMD check` reports no unstated dependencies in the tests or the vignette.
Examples, tests (`testthat.R`), vignette rebuild, and both the PDF and the
HTML manual all passed.

## New dependency

'survey' (>= 4.5) has been added to Imports. It is what computes the
design-based standard errors, confidence intervals and Kaplan-Meier bands when
the user sets a survey weight; nothing else in the package needs it, and the
figures that do not involve a weight do not call it.

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

## Writing to the file system

The package writes nothing on load or on attach, and no example or vignette
chunk writes at all. Every test that exercises the export path writes under
`tempdir()` and removes what it wrote.

The one place the package writes a file the user keeps is the application's
Export button. The output folder is a text field in the UI, filled in with
`figures` and editable before anything is written, and the folder is created
and the figures written only when the user presses Export in an interactive
session. Nothing is written when the application merely opens, and the full
path of what was written is reported back on screen afterwards.

## Test suite

`R CMD check --as-cran` on Windows 11 x64 with R 4.6.0 ran the suite in 212
seconds: 1349 passing, 0 failures, 0 warnings, 0 skips.

The preview subsampling caps are one million rows (scatter and line plots) and
ten million rows (the plot types that summarise their rows before drawing).
The tests exercise the sampling rules through an injected threshold rather
than by allocating a table of that size, so the suite stays within a check
machine's memory.

## Reverse dependencies

There are none.
