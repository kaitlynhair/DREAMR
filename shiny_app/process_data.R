# Function to Process OpenAlex Data for Visualization
process_oa_data <- function(data, global_south_country_codes) {
  library(dplyr)
  library(tidyr)
  library(openalexR)
  
  # Convert doi to lower case
  if("doi" %in% colnames(data)){
    data$doi <- tolower(data$doi)
  }
  
  # Create empty results dataframe
  oa_results <- data.frame()
  
  # Filter records with doi and fetch
  if("doi" %in% colnames(data) & nrow(filter(data, !is.na(doi)))){
    data_doi <- data %>% filter(!is.na(doi)) %>%
      mutate(identifier = paste0("doi:",doi))
    oa_result <- oa_metadata(data_doi, identifier="doi")
    if(!"ab" %in% colnames(oa_result)){
      oa_result <- oa_result %>% mutate(ab = NA)
    }
    oa_results <- rbind(oa_result, oa_results)
  }
  
  # Remove found from data
  if("doi" %in% colnames(data)){
    data <- data %>% filter(!doi %in% oa_results$doi)
  }
  
  # Filter records with pmid and fetch
  if("pmid" %in% colnames(data) & nrow(filter(data, !is.na(pmid))) > 0){
    data_pmid <- data %>% filter(!is.na(pmid)) %>%
      mutate(identifier = paste0("pmid:",pmid))
    oa_result <- oa_metadata(data_pmid, identifier= "pmid")
    if(!"ab" %in% colnames(oa_result)){
      oa_result <- oa_result %>% mutate(ab = NA)
    }
    oa_results <- rbind(oa_result, oa_results)
  }
  
  # Remove found from data
  if("pmid" %in% colnames(data)){
    data <- data %>% filter(!pmid %in% oa_results$pmid)
  }
  
  # Filter records with pmcid and fetch
  if("pmcid" %in% colnames(data) & nrow(filter(data, !is.na(pmcid))) > 0){
    data_pmcid <- data %>% filter(!is.na(pmcid)) %>%
      mutate(identifier = paste0("pmcid:",pmcid))
    oa_result <- oa_metadata(data_pmcid, identifier= "pmcid")
    if(!"ab" %in% colnames(oa_result)){
      oa_result <- oa_result %>% mutate(ab = NA)
    }
    oa_results <- rbind(oa_result, oa_results)
  }
  
  
  # Get funders
  funders <- extract_funder(oa_results)
  funders <- soles::format_doi(funders)
  funders <- funders %>%
    filter(!funder_name =="Unknown") %>%
    group_by(id) %>%
    slice_head() %>%
    ungroup() %>%
    select(id, funder_name)
  
  # Get institutions - first author
  institutions_first_author <- extract_institution(oa_results, author_position = "first")
  institutions_first_author <- institutions_first_author %>%
    filter(!institution_id =="Unknown") %>%
    group_by(id) %>%
    slice_head() %>%
    select(name, doi, institution_country_code) %>%
    rename(last_author_institution = name, first_author_country=institution_country_code)
  
  # Get institutions - last author
  institutions_last_author <- extract_institution(oa_results, author_position = "last")
  institutions_last_author <- institutions_last_author %>%
    filter(!institution_id =="Unknown") %>%
    group_by(id) %>%
    slice_head() %>%
    select(name, doi, institution_country_code) %>%
    rename(last_author_institution = name, last_author_country=institution_country_code)
  
  # Career stage - first author
  first_author_career_stage <- oa_results %>%
    tidyr::unnest(author) %>%
    select(id, au_id, publication_year, author_position, au_orcid) %>%
    filter(author_position=="first")
  
  # Career stage - last author
  last_author_career_stage <- oa_results %>%
    tidyr::unnest(author) %>%
    select(id, au_id, publication_year, author_position, au_orcid) %>%
    filter(author_position=="last")
  
  authors_all1 <- openalexR::oa_fetch(entity="authors",
                                      orcid = first_author_career_stage$au_orcid)
  
  authors_all2 <- openalexR::oa_fetch(entity="authors",
                                      orcid = last_author_career_stage$au_orcid)
  
  authors_all_formatted1 <- authors_all1 %>%
    rename(au_orcid = orcid) %>%
    select(au_orcid, counts_by_year) %>%
    tidyr::unnest(counts_by_year) %>%
    group_by(au_orcid) %>%
    arrange(year) %>%
    slice_head() %>%
    rename(first_year_active = year)
  
  authors_all_formatted2 <- authors_all2 %>%
    rename(au_orcid = orcid) %>%
    select(au_orcid, counts_by_year) %>%
    tidyr::unnest(counts_by_year) %>%
    group_by(au_orcid) %>%
    arrange(year) %>%
    slice_head() %>%
    rename(first_year_active = year)
  
  first_author_career_stage <- first_author_career_stage %>%
    left_join(authors_all_formatted1) %>%
    mutate(first_author_years_active = publication_year - first_year_active) %>%
    select(first_author_years_active, id)
  
  last_author_career_stage <- last_author_career_stage %>%
    left_join(authors_all_formatted2) %>%
    mutate(last_author_years_active = publication_year - first_year_active) %>%
    select(last_author_years_active, id)
  
  # Global south classification
  global_south_first_author <- extract_institution(oa_results, author_position = "first") %>%
    mutate(first_author_global_south = ifelse(institution_country_code %in% global_south_country_codes, TRUE, FALSE)) %>%
    select(doi, id, institution_id, first_author_global_south) %>%
    unique()
  
  global_south_last_author <- extract_institution(oa_results, author_position = "last") %>%
    mutate(last_author_global_south = ifelse(institution_country_code %in% global_south_country_codes, TRUE, FALSE)) %>%
    select(doi, id, institution_id, last_author_global_south) %>%
    unique()
  
  # Merging final results
  oa_results_final <- oa_results %>%
    select(title, doi, id, is_oa, language, publication_year, cited_by_count) %>%
    left_join(first_author_career_stage, by="id") %>%
    left_join(last_author_career_stage, by="id") %>%
    left_join(institutions_last_author) %>%
    left_join(institutions_first_author) %>%
    left_join(global_south_first_author) %>%
    left_join(global_south_last_author) %>%
    left_join(funders, by="id") %>%
    unique()
  
  return(oa_results_final)
}

# Example Usage
global_south_country_codes <- c(
  # Africa
  "DZ", "AO", "BJ", "BW", "BF", "BI", "CM", "CV", "CF", "TD",
  "KM", "CG", "CD", "DJ", "EG", "GQ", "ER", "ET", "GA", "GM",
  "GH", "GN", "GW", "CI", "KE", "LS", "LR", "LY", "MG", "MW",
  "ML", "MR", "MU", "MA", "MZ", "NA", "NE", "NG", "RW", "ST",
  "SN", "SC", "SL", "SO", "ZA", "SS", "SD", "TZ", "TG", "TN",
  "UG", "ZM", "ZW",
  
  # Asia
  "AF", "AM", "AZ", "BD", "BT", "KH", "CN", "GE", "IN", "ID",
  "IR", "IQ", "JO", "KZ", "KG", "LA", "LB", "MY", "MV", "MN",
  "MM", "NP", "PK", "PH", "SY", "TJ", "TH", "TM", "UZ", "VN", "YE",
  
  # Latin America and the Caribbean
  "AR", "BS", "BB", "BZ", "BO", "BR", "CL", "CO", "CR", "CU",
  "DM", "DO", "EC", "SV", "GD", "GT", "GY", "HT", "HN", "JM",
  "MX", "NI", "PA", "PY", "PE", "KN", "LC", "VC", "SR", "TT", "UY", "VE",
  
  # Oceania
  "AS", "CK", "FJ", "PF", "GU", "KI", "MH", "FM", "NR", "NC",
  "NU", "PW", "PG", "WS", "SB", "TK", "TO", "TV", "VU", "WF",
  
  # Middle East
  "BH", "IR", "IQ", "JO", "KW", "LB", "OM", "PS", "QA", "SA", "SY", "YE"
)


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
  
  
  identifier_col <- "identifier"
  
  # Create a dataframe with data from OpenAlex
  for (i in seq_along(data[[identifier_col]])) {
    suppressWarnings({
      try(new <- openalexR::oa_fetch(
        identifier = data[[identifier_col]][i],
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
  
  Sys.sleep(2) # adding a 2 second system sleep between calls to avoid API limits
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
      id,
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
      id,
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
    dplyr::select(id, doi, grants) %>%
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
      select(id, doi, funder_name, award_id, method)
    
  }
  # Step 3: Handle DOIs with missing funder information
  res_funder_failed <- data %>%
    dplyr::filter(!doi %in% res_funder$doi) %>%
    dplyr::mutate(
      funder_name = "Unknown",
      award_id = "Unknown",
      method = "OpenAlex"
    ) %>%
    select(id, doi, funder_name, award_id, method)
  
  # Step 4: Combine successful and failed funder data
  res_funder <- dplyr::bind_rows(res_funder, res_funder_failed)
  
  
  return(res_funder)
}

