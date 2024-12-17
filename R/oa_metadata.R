#' Extract metadata from OpenAlex for a list of identifiers
#'
#' This function retrieves metadata (e.g., concepts, funders, citation count, and more)
#' from OpenAlex for a given set of publication identifiers such as DOIs, PMIDs, or PMCIDs.
#'
#' @param data A data frame containing publication identifiers.
#' @param identifier A character vector specifying the type of identifier to query
#' (e.g., "pmid", "doi", or "pmcid"). Defaults to "pmid".
#'
#' @importFrom dplyr bind_rows
#' @importFrom openalexR oa_fetch
#' @return A data frame containing metadata retrieved from OpenAlex for the provided identifiers.
#' If no metadata is retrieved, returns NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' publication_data <- data.frame(doi = c("10.1038/nature12373", "10.1126/science.169.3946.635"))
#' metadata <- oa_metadata(publication_data, identifier = "doi")
#' }
oa_metadata <- function(data, identifier = c("pmid", "doi", "pmcid")) {
  res <- NULL

  # Create a dataframe with data from OpenAlex
  for (i in seq_along(data[[identifier]])) {
    suppressWarnings({
      try(new <- openalexR::oa_fetch(
        identifier = data[[identifier]][i],
        entity = "works"
      ), silent = TRUE)
    })
    if (is.data.frame(new)) {
      res <- dplyr::bind_rows(res, new)
    }
  }

  if (is.null(res)) {
    message("Couldn't tag any more records.")
  }

  return(res)
}


#' Extract institution information for a specified author position
#'
#' This function extracts institution metadata for authors from a dataset
#' based on their position (e.g., "first" or "last" author).
#'
#' @param data A data frame containing author and institution metadata, typically retrieved from OpenAlex.
#' @param author_position A character string specifying the author position to filter by
#' (e.g., "first", "last"). Defaults to "first".
#'
#' @importFrom dplyr filter select mutate bind_rows
#' @importFrom tidyr unnest
#' @importFrom stringr str_remove
#' @return A data frame containing institution information for the specified author position,
#' including fields like institution ID, display name, ROR, country code, and type.
#' Missing or unavailable data is replaced with "Unknown"
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' institution_data <- extract_institution(data, author_position = "first")
#' }
extract_institution <- function(data, author_position = "first") {

  # Extract institution information for the specified author position
  res_institution <- data %>%
    tidyr::unnest(author) %>%
    dplyr::filter(author_position == !!author_position) %>%
    dplyr::select(
      doi,
      institution_id,
      name = institution_display_name,
      ror = institution_ror,
      institution_country_code,
      type = institution_type
    ) %>%
    dplyr::mutate(
      institution_country_code = toupper(institution_country_code),
      method = "OpenAlex"
    ) %>%
    replace(is.na(.), "Unknown")

  # Handle DOIs with missing institution data
  res_institution_failed <- data %>%
    dplyr::filter(!doi %in% res_institution$doi) %>%
    dplyr::mutate(
      institution_id = "Unknown",
      name = "Unknown",
      ror = "Unknown",
      institution_country_code = "Unknown",
      type = "Unknown",
      method = "OpenAlex"
    ) %>%
    dplyr::select(
      doi,
      institution_id,
      name,
      ror,
      institution_country_code,
      type
    )

  # Return both successful and failed results combined
  dplyr::bind_rows(res_institution, res_institution_failed)
}

#' Extract funder information and grant IDs from OpenAlex results
#'
#' This function processes metadata to extract funder names and associated grant award IDs
#' for publications, ensuring the data is cleaned and missing information is handled.
#'
#' @param data A data frame containing metadata, including DOIs and funder information#'
#' @importFrom dplyr select mutate filter bind_rows
#' @importFrom tidyr unnest_longer
#' @importFrom stringr str_remove
#' @return A data frame with extracted funder names, award IDs, and associated DOIs.
#' Missing data is filled with "Unknown".
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' funder_data <- extract_funder(res, funder_full, citations_missing_data)
#' }
extract_funder <- function(data) {

    # Step 1: Extract funder information and unnest grants
    res_funder <- data %>%
      dplyr::select(doi, grants) %>%
      dplyr::mutate(doi = stringr::str_remove(doi, "https://doi.org/")) %>%
      tidyr::unnest_longer(grants) %>%
      dplyr::filter(!is.na(grants))

    # Step 2: Process funder information if rows are present
    if (nrow(res_funder) > 0) {
      # Remove "funder" entries and initialize award_id
      res_funder <- res_funder %>%
        dplyr::filter(grants_id != "funder") %>%
        dplyr::mutate(award_id = NA)

      # Assign award IDs to their corresponding grants
      if (nrow(res_funder) > 1) {
        for (i in 1:(nrow(res_funder) - 1)) {
          if (res_funder$grants_id[i + 1] == "award_id") {
            res_funder$award_id[i] <- res_funder$grants[i + 1]
          }
        }
      }

      # Filter out award_id rows and clean up the data
      res_funder <- res_funder %>%
        dplyr::filter(grants_id != "award_id") %>%
        dplyr::select(-grants_id, funder_name = grants, doi) %>%
        dplyr::mutate(method = "OpenAlex") %>%
        replace(is.na(.), "Unknown") %>%
        select(doi, funder_name, award_id, method)

    }
      # Step 3: Handle DOIs with missing funder information
      res_funder_failed <- data %>%
        dplyr::filter(!doi %in% res_funder$doi) %>%
        dplyr::mutate(
          funder_name = "Unknown",
          award_id = "Unknown",
          method = "OpenAlex"
        ) %>%
        select(doi, funder_name, award_id, method)

      # Step 4: Combine successful and failed funder data
      res_funder <- dplyr::bind_rows(res_funder, res_funder_failed)


    return(res_funder)
  }

