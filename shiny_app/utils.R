normalize_doi <- function(x) {
  x <- tolower(x)
  x <- sub("^https?://doi.org/", "", x)
  x <- sub("^doi:", "", x)
  x
}