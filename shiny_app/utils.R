normalize_doi <- function(x) {
  x |>
    tolower() |>
    sub("^https?://doi.org/", "") |>
    sub("^doi:", "")
}