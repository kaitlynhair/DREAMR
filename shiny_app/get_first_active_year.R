#' Get the first active publication year from an ORCID iD
#'
#' This function queries the ORCID public API for a given ORCID iD
#' and extracts the earliest available publication year from the works list.
#'
#' @param orcid_id A character string with a valid ORCID iD
#'   (e.g., `"0000-0001-9187-9839"`).
#'
#' @return A character string with the earliest publication year
#'   (e.g., `"2015"`) or `"Unknown"` if no valid publication years
#'   are available or if the ORCID iD / API request fails.
#'
#' @examples
#' \dontrun{
#' get_first_active_year("0000-0001-9187-9839")
#' get_first_active_year("0000-0000-0000-0000") # invalid ORCID, returns "Unknown"
#' }
get_first_active_year <- function(orcid_id) {
  # Build API URL
  url <- paste0("https://pub.orcid.org/v3.0/", orcid_id, "/works")

  # Fetch works (set Accept header to JSON)
  res <- httr::GET(url, httr::add_headers(Accept = "application/json"))

  if (httr::status_code(res) != 200) {
    return("Unknown")
  }

  works <- httr::content(res, as = "parsed", type = "application/json")

  # Extract list of works
  work_groups <- works$group

  # Get all publication years
  years <- purrr::map(work_groups, function(g) {
    summary <- g$`work-summary`[[1]]
    date <- summary$`publication-date`
    if (!is.null(date$year$value)) {
      return(as.integer(date$year$value))
    } else {
      return(NA_integer_)
    }
  }) %>% unlist()

  # Drop NAs
  years <- years[!is.na(years)]

  if (length(years) == 0) {
    return("Unknown")
  }

  return(as.character(min(years)))  # always return character
}
