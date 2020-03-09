
str_check_coverage <- function(x, pattern) {
  missed <- stringr::str_subset(x, pattern, negate = TRUE)
  prop <- signif(length(missed) / length(x), 2) * 100
  if (length(missed) == 0) {
    message("all cased covered")
  } else {
    examples <- glue::glue_collapse(head(missed), sep = ", ", width = 40)
    message(glue::glue("{prop}% of cases not covered, including: {examples}"))
  }
  invisible(missed)
}

scrub_pattern <- function(x, pattern) {
  new_x <- stringr::str_remove(x, pattern)
  stopifnot(all(x != new_x))
  new_x
}
