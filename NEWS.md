# ggstratify 0.0.1

* Initial release.

* `ggstratify()` takes a data frame or a matrix that is already in your
  session, and nothing else. There is no file-upload control in the
  application and a file path is no longer accepted as `dataset`. Data read
  from disk is typed by whatever the reader guessed, and would then be
  described on those guesses; read the file yourself, look at the result, and
  pass the object.

* The name the data was passed under is what the generated code refers to it
  by, so `ggstratify(cohort)` prints code that starts `d <- cohort`.

* Previews are subsampled only above one million rows for scatter and line
  plots and ten million for the plot types that summarise their rows first,
  and never for Kaplan-Meier curves.
