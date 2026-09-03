# ggstratify 0.1.0

* A numeric variable is now treated as continuous because of its type, not
  because it has enough distinct values. Counting distinct values guessed
  wrong on measurements that are recorded coarsely -- an age in whole years
  over a narrow range, a visit number, a score out of five -- and refused to
  plot them on an axis. A column holding at most two values is still not
  offered as a measurement: an indicator coded 0/1 cannot describe a spread.
  A few-levelled numeric column can now be both, plotted on an axis and
  stratified by.

* A line plot's **Mark the observations** option has moved from **Plot
  options** to **Describe**, beside the rest of the line settings, and draws
  its points at `size = 1` so that a dense set of them does not cover the
  line it belongs to.

* **Derive a variable** (the panel formerly called Categorize) gains a fourth
  method: *missing vs observed*. It reads no values, only whether there is a
  value, so it applies to a variable of any type -- a factor, a character
  column or a date as much as a number -- and turns it into two groups that
  can be used as a layer like any other categorical variable.

* Rows with a missing value in a layer variable are excluded, as before. A
  missing-vs-observed variable is the one layer that never has one: `is.na()`
  answers for every row. The rows another layer would have dropped are
  therefore exactly the ones this puts in front of you, which is what makes it
  a way to look at who is not in the data rather than only to count them.

* Describing a variable within its own missingness leaves the Missing figure
  with nothing to draw. The app reports that arrangement under **Layers**
  instead of drawing an empty figure without comment, and still draws it.

* A variable with no missing values, or with nothing but missing values, gives
  one group rather than two and is refused with a note saying which.

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
