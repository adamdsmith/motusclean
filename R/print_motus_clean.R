#' Print a motus_clean object
#'
#' @param x A `motus_clean` object.
#' @param ... Additional arguments, currently ignored.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.motus_clean <- function(x, ...) {

  o <- x$summary$overall

  cat(
    "Motus detection preparation\n\n",
    "Sources:       ", o$sources, "\n",
    "Raw runs:      ", o$raw_runs, "\n",
    "Eligible runs: ", o$eligible_runs, "\n",
    "  PASS:        ", o$pass_runs, "\n",
    "  REVIEW:      ", o$review_runs, "\n",
    "Excluded runs: ", o$excluded_runs, "\n",
    sep = ""
  )

  invisible(x)
}
