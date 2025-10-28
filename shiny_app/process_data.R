dreamr_extract_cached <- function(refdata, cache_dir = ".cache", hash_length = 8, progress = NULL) {
  # Ensure .cache folder exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }

  # Generate hash of refdata
  hash <- digest(refdata, algo = "xxhash64")
  short_hash <- substr(hash, 1, hash_length)

  # Look for matching file in cache
  files <- list.files(cache_dir, pattern = paste0("^", short_hash), full.names = TRUE)

  if (length(files) > 0) {
    message("✅ Cache hit: Loading ", basename(files[[1]]))
    return(readRDS(files[[1]]))
  }

  # Otherwise, compute and save
  message("⏳ Cache miss: Running dreamr_extract() and saving to cache")
  result <- dreamr_extract(refdata, progress = progress)

  # Save with full hash in filename
  saveRDS(result, file = file.path(cache_dir, paste0(short_hash, "_oa_data.rds")))

  return(result)
}


#' Extract Paper, Author, and Institution Data from OpenAlex
#'
#' This function retrieves and processes OpenAlex data for a given set of papers.
#' It enriches the data with funder information, research domain, author details,
#' and institution information, including country mapping.
#'
#' @param data Input dataset or query for `pull_openalex()`.
#' @return A list with three elements:
#'   \describe{
#'     \item{pub_metadata}{Data frame of paper-level metadata (title, DOI, journal, publication year, OA status, funder, domain, etc.)}
#'     \item{institutions}{Data frame of institution-level data (affiliation ID, country, source, etc.)}
#'     \item{authors}{Data frame of author-level data (author ID, position, ORCID, years since first publication, source, etc.)}
#'   }
#' @examples
#' @importFrom countrycode countrycode
#' \dontrun{
#' results <- dreamr_extract(my_query)
#' head(results$pub_metadata)
#' head(results$authors)
#' head(results$institutions)
#' }
#' @export
dreamr_extract <- function(data, progress = NULL) {
  p <- function(detail, amount) {
    if (!is.null(progress)) {
      try(progress(detail, amount), silent = TRUE)
    }
  }

  # Pull raw OpenAlex results 
  p("Fetching OpenAlex records", 0.15)
  oa_results <- pull_openalex(data)

  # Add funder information
  p("Extracting funder information", 0.15)
  funders <- extract_funder(oa_results)

  # Add research domain information
  p("Classifying research domains", 0.10)
  domains <- extract_domain(oa_results)

  oa_results <- left_join(oa_results, funders)
  oa_results <- left_join(oa_results, domains)

  # Extract institution-level details
  p("Extracting institution data", 0.10)
  institutions <- extract_institution(oa_results) %>%
    filter(!affilitation_id == "Unknown") %>%
    mutate(source = "OpenAlex API") %>%
    mutate(country = countrycode::countrycode(country_code, origin = "iso2c", destination = "country.name")) %>%
    distinct()
  

  # Extract author-level details
  authors <- oa_results %>%
    select(-display_name) %>%
    rename(openalex_id = id) %>%
    tidyr::unnest(authorships) %>%
    select(openalex_id, id, publication_year, author_position, orcid, display_name) %>%
    mutate(
      orcid = gsub("https://orcid.org/", "", orcid),
      first_name = stringr::word(display_name, 1)
    ) %>%
    rename(author_id = id) %>%
    mutate(source = "ORCID API") %>%
    distinct()
  
  # De-duplicate ORCIDs before calling slow function
  unique_orcids <- unique(na.omit(authors$orcid))
  orcid_progress_per_id <- 0.15 / length(unique_orcids)
  # Lookup only once per ORCID
  orcid_years <- purrr::map_dfr(
    seq_along(unique_orcids),
    function(i) {
      o <- unique_orcids[[i]]
      p(paste0("Processing ORCID ", i, "/", length(unique_orcids)), orcid_progress_per_id)
      tibble(
        orcid = o,
        first_active_year = get_first_active_year(o)
      )
    }
  )
  
  # Join back
  authors <- authors %>%
    left_join(orcid_years, by = "orcid") %>%
    mutate(
      years_sice_first_pub = ifelse(
        first_active_year == "Unknown",
        "Unknown",
        as.numeric(publication_year) - as.numeric(first_active_year)
      )
    )
  

  # Add gender data
  p("Running first names through gender R package", 0.10)
  unique_first_names <- unique(authors$first_name)
  gender_data <- gender(unique_first_names, method = "ssa") %>%
    select(name, gender)  # Keep only name and gender
  authors <- authors %>%
    left_join(gender_data, by = c("first_name" = "name"))


  # Prepare publication-level metadata
  p("Structuring publication metadata", 0.15)
  pub_metadata <- oa_results %>%
    dplyr::select(
      id,                        # OpenAlex ID
      title,                     # Title of work
      doi,                       # DOI
      publication_year,          # Year
      language,                  # Language
      type,                      # Type of publication (article, book, etc.)
      source_display_name,       # Journal / source name
      issn_l,                    # ISSN
      host_organization_name,    # Publisher / hosting org
      is_oa,                     # Open access?
      oa_status,                 # OA category (gold, green, etc.)
      oa_url,                    # Direct OA URL
      cited_by_count,            # Citation count
      fwci,                      # Field-weighted citation impact (if populated)
      funder_name,               # From added join
      domain                     # From added join
    ) %>%
    mutate(source = "OpenAlex API") %>%
    distinct()

  # Return structured results
  p("Finalizing results", 0.1)
  return(list(
    pub_metadata = pub_metadata,
    institutions = institutions,
    authors = authors
  ))
}


#' Pull and Process OpenAlex Metadata
#'
#' This function takes a dataframe containing publication identifiers
#' (DOI, PMID, PMCID) and retrieves corresponding metadata from the
#' OpenAlex API using the `openalexR` package. It processes and returns
#' a deduplicated dataframe suitable for visualization or further analysis.
#'
#' @param data A dataframe that may contain columns `"doi"`, `"pmid"`,
#'   and/or `"pmcid"`. At least one of these identifiers should be present.
#'   DOIs will be automatically lowercased and formatted using
#'   `soles::format_doi()`.
#'
#' @return A dataframe of OpenAlex metadata records. If available, the
#'   metadata will include abstracts (`ab`). The result is deduplicated
#'   by identifier and may contain fewer rows than the input dataframe
#'   (because only records with valid identifiers are retrieved).
#'
#' @details
#' The function sequentially attempts to fetch metadata by:
#' \enumerate{
#'   \item DOI (if present in the input data)
#'   \item PMID
#'   \item PMCID
#' }
#'
#' For each identifier type, records that have already been matched are
#' excluded from subsequent queries to avoid duplicates. If abstracts
#' are not provided by OpenAlex, an empty column `ab` is added and filled
#' with `NA`.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Example input data
#' input_data <- tibble::tibble(
#'   doi = c("10.1038/s41586-020-2649-2", NA),
#'   pmid = c(NA, "32759994"),
#'   pmcid = c(NA, NA)
#' )
#'
#' results <- pull_openalex(input_data)
#' head(results)
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import openalexR
#' @export
pull_openalex <- function(data) {
  library(dplyr)
  library(tidyr)
  library(openalexR)

  # Convert doi to lower case
  if ("doi" %in% colnames(data)) {
    data$doi <- tolower(data$doi)
    data <- format_doi(data)
  }

  # Create empty results dataframe
  oa_results <- data.frame()

  # Filter records with doi and fetch
  if ("doi" %in% colnames(data) & nrow(filter(data, !is.na(doi)))) {
    data_doi <- data %>% filter(!is.na(doi)) %>%
      mutate(identifier = paste0("doi:", doi))
    oa_result <- oa_metadata(data_doi, identifier = "doi")

    # If no abstract, make it NA
    if (!"abstract" %in% colnames(oa_result)) {
      oa_result <- oa_result %>% mutate(abstract = NA)
    }
    oa_results <- rbind(oa_result, oa_results)
  }
  # Remove found from data
  if ("doi" %in% colnames(data)) {
    data <- data %>% filter(!doi %in% oa_results$doi)
  }
  
  # Filter records with pmid and fetch
  if ("pmid" %in% colnames(data)) {
    if(nrow(filter(data, !is.na(pmid))) > 0) {
      
      data_pmid <- data %>% filter(!is.na(pmid)) %>%
        mutate(identifier = paste0("pmid:", pmid))
      oa_result <- oa_metadata(data_pmid, identifier = "pmid")
      if (!"abstract" %in% colnames(oa_result)) {
        oa_result <- oa_result %>% mutate(abstract = NA)
      }
      oa_results <- rbind(oa_result, oa_results)
      
    }
  }
  # Remove found from data
  if ("pmid" %in% colnames(oa_results) & "pmid" %in% colnames(data)) {
    data <- data %>%
      mutate(pmid = ifelse(is.na(pmid), "", pmid)) %>%
      filter(!pmid %in% oa_results$pmid)
  }
  # Filter records with pmcid and fetch
  if ("pmcid" %in% colnames(data)) {
    
    if(nrow(filter(data, !is.na(pmcid))) > 0) {
      data_pmcid <- data %>% filter(!is.na(pmcid)) %>%
        mutate(identifier = paste0("pmcid:", pmcid))
      oa_result <- oa_metadata(data_pmcid, identifier = "pmcid")
      if (!"abstract" %in% colnames(oa_result)) {
        oa_result <- oa_result %>% mutate(abstract = NA)
      }
      if (!"license" %in% colnames(oa_result)) {
        oa_result <- oa_result %>% mutate(license = NA)
      }
      oa_results <- rbind(oa_result, oa_results)
      
    }
  }
  return(oa_results)

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


#' Extract institution information for a specified author position
#'
#' This function extracts institution metadata for authors from a dataset
#' based on their position (e.g., "first" or "last" author).
#'
#' @param data A data frame containing author and institution metadata, typically retrieved from OpenAlex.
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
extract_institution <- function(data) {
  # Extract institution information for the specified author position
  res_institution <- data %>%
   select(-display_name, -type) %>%
    rename(openalex_id = id) %>%
    tidyr::unnest(authorships) %>%
    dplyr::select(
      author_name = display_name,
      openalex_id,
      doi,
      author_id = id,
      affiliations,
      author_position) %>%
    unnest(affiliations) %>%
    rename(affilitation_id = id) %>%
    dplyr::mutate(
      country_code = toupper(country_code),
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
      country_code = "Unknown",
      type = "Unknown",
      method = "OpenAlex"
    ) %>%
    dplyr::select(
      doi,
      ror,
      country_code,
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

  res_funder <- format_doi(res_funder)
  res_funder <- res_funder %>%
    filter(!funder_name =="Unknown") %>%
    group_by(id) %>%
    slice_head() %>%
    ungroup() %>%
    select(id, funder_name)

  return(res_funder)
}


#' Extract top-level OpenAlex domain(s) for each work
#'
#' This function takes OpenAlex metadata (from `oa_metadata()` or `pull_openalex()`)
#' and extracts the highest-level domain (level 0) or top-scoring concept if no
#' level 0 domain is present. It then merges this back into the main dataframe.
#'
#' @param oa_results A dataframe of OpenAlex metadata containing a `concepts` list-column.
#' @param top_n Integer. Number of top concepts/domains to return (default = 1).
#'
#' @return A dataframe with an added `domain` column (character).
#'
#' @examples
#' \dontrun{
#' oa <- pull_openalex(data_with_dois)
#' oa_with_domain <- add_openalex_domain(oa)
#' head(oa_with_domain$domain)
#' }
extract_domain <- function(oa_results, top_n = 1) {
  if (!"concepts" %in% colnames(oa_results)) {
    warning("No 'concepts' column found in oa_results — returning unchanged dataframe.")
    return(oa_results)
  }

  domains <- oa_results %>%
    select(id, topics) %>%
    rename(openalex_id = id) %>%
    tidyr::unnest(topics) %>%
    filter(type == "domain") %>%
    dplyr::arrange(openalex_id, dplyr::desc(score)) %>%
    dplyr::group_by(openalex_id) %>%
    dplyr::slice_head() %>%
    dplyr::summarise(domain = paste(unique(display_name), collapse = "; "),
                     .groups = "drop") %>%
    rename(id = openalex_id)

}


#' Summarize missing data across OpenAlex output
#'
#' @param refdata Original uploaded references
#' @param oa_list Output of dreamr_extract(refdata), a list with pub_metadata, authors, institutions
#' @param key_cols List of character vectors specifying key columns to check per component.
#'        Default: list(pub_metadata = c("doi","pmid","funder_name","domain"),
#'                      authors = c("orcid","first_active_year"),
#'                      institutions = c("affilitation_id","institution_name"))
#' @return A list of summaries for pub_metadata, authors, and institutions
#' Summarize missing data across OpenAlex output
#'
#' @param refdata Original uploaded references
#' @param oa_list Output of dreamr_extract(refdata), a list with pub_metadata, authors, institutions
#' @param key_cols List of character vectors specifying key columns to check per component.
#'        Default: list(pub_metadata = c("doi","pmid","funder_name","domain"),
#'                      authors = c("orcid","first_active_year"),
#'                      institutions = c("affilitation_id","institution_name"))
#' @return A list of summaries for pub_metadata, authors, and institutions
summarize_oa_missing <- function(refdata, oa_list,
                                 key_cols = list(
                                   pub_metadata = c("doi","pmid","funder_name","domain"),
                                   authors = c("orcid","first_active_year"),
                                   institutions = c("affilitation_id","institution_name")
                                 )) {

  result <- list()

  for(component in c("pub_metadata", "authors", "institutions")) {
    df <- oa_list[[component]]

    # Default empty data frame if NULL
    if(is.null(df)) df <- data.frame()

    total_records <- nrow(refdata)

    # Calculate records not found in OpenAlex
    if(component == "pub_metadata"){

      # Ensure doi/pmid exist in both refdata and retrieved
      for(col in c("doi","pmid")){
        if(!col %in% colnames(df)) df[[col]] <- NA_character_
        if(!col %in% colnames(refdata)) refdata[[col]] <- NA_character_
      }

      retrieved_ids <- unique(c(df$doi, df$pmid))
      original_ids <- unique(c(refdata$doi, refdata$pmid))
      not_found_count <- sum(!original_ids %in% retrieved_ids)
    } else {
      # For authors/institutions, assume all correspond to refdata
      not_found_count <- max(0, nrow(refdata) - nrow(df))
    }

    retrieved <- nrow(df)

    # Calculate per-field missingness for retrieved records
    if(nrow(df) == 0){
      missing_summary <- data.frame(
        field = key_cols[[component]],
        count_missing = rep(NA_integer_, length(key_cols[[component]])),
        percentage_missing = NA_real_
      )
    } else {
      # Ensure key columns exist
      for(col in key_cols[[component]]){
        if(!col %in% colnames(df)) df[[col]] <- NA
      }

      # Replace NA/empty with "Unknown"
      df_clean <- df %>%
        dplyr::mutate(dplyr::across(all_of(key_cols[[component]]),
                                    ~ifelse(is.na(.) | . == "", "Unknown", .)))

      missing_summary <- sapply(df_clean[key_cols[[component]]], function(x){
        sum(x == "Unknown")
      }) %>%
        data.frame(field = names(.),
                   count_missing = as.integer(.),
                   stringsAsFactors = FALSE)

      missing_summary$percentage_missing <- round((missing_summary$count_missing / retrieved) * 100, 1)
    }

    result[[component]] <- list(
      total_records = total_records,
      retrieved = retrieved,
      not_found_in_oa = not_found_count,
      missing_summary = missing_summary
    )
  }

  return(result)
}


oa_field_completeness_table <- function(oa_list) {

  rows <- list()

  for(component in c("pub_metadata", "authors", "institutions")) {

    df <- oa_list[[component]]

    # Skip if nothing retrieved
    if(is.null(df) || nrow(df) == 0) next

    # Select key columns depending on component
    key_cols <- switch(component,
                       pub_metadata = c("doi","pmid", "is_oa", "oa_status", "language", "funder_name","domain"),
                       authors = c("orcid","first_active_year"),
                       institutions = c("affilitation_id","display_name", "ror", "author_position"))

    # Ensure all columns exist
    for(col in key_cols){
      if(!col %in% colnames(df)) df[[col]] <- NA
    }

    # Replace NA/empty with "Unknown"
    df_clean <- df %>%
      dplyr::mutate(dplyr::across(all_of(key_cols),
                                  ~ifelse(is.na(.) | . == "", "Unknown", .)))

    # Count missing per field
    missing_summary <- sapply(df_clean[key_cols], function(x){
      sum(x == "Unknown")
    })

    # Build descriptive "Retrieved_records" column
    retrieved_text <- switch(component,
                             pub_metadata = "papers retrieved",
                             authors = "authors retrieved",
                             institutions = "institutions retrieved")

    summary_df <- data.frame(
      Component = component,
      Field = names(missing_summary),
      Retrieved_records = paste0(nrow(df), " ", retrieved_text),
      Missing_count = as.integer(missing_summary),
      Percent_missing = round(as.integer(missing_summary) / nrow(df) * 100, 1),
      stringsAsFactors = FALSE
    )

    rows[[length(rows) + 1]] <- summary_df
  }

  summary_table <- dplyr::bind_rows(rows)

  return(summary_table)
}


oa_retrieval_summary <- function(refdata, oa_list) {

  total_records <- nrow(refdata)

  pub_metadata <- oa_list$pub_metadata
  retrieved <- if(is.null(pub_metadata)) 0 else nrow(pub_metadata)

  percent_retrieved <- round(retrieved / total_records * 100, 1)

  summary_df <- data.frame(
    Total_records = total_records,
    Retrieved_in_OpenAlex = retrieved,
    Percent_retrieved = percent_retrieved,
    stringsAsFactors = FALSE
  )

  return(summary_df)
}


oa_metadata <- function(data, identifier = c("pmid", "doi", "pmcid"), 
                        batch_size = 200, sleep = 0.5) {
  identifier <- match.arg(identifier)
  identifier_col <- identifier
  
  ids <- data[[identifier_col]]
  if (is.null(ids)) {
    stop("Column '", identifier, "' not found in data.")
  }
  
  # Prefix IDs with type (OpenAlex expects e.g., doi:10.xxx)
  ids_prefixed <- paste0(identifier, ":", ids)

  # Split into batches
  id_batches <- split(ids_prefixed, ceiling(seq_along(ids_prefixed) / batch_size))
  
  results <- purrr::map_dfr(id_batches, function(batch) {
    purrr::map_dfr(batch, function(single_id) {
      ans <- try(
        openalexR::oa_fetch(
          identifier = single_id,
          entity = "works"
        ),
        silent = TRUE
      )
      
      if (!inherits(ans, "try-error") && is.data.frame(ans)) {
        return(ans)
      } else {
        return(NULL)
      }
    })
  })
  
  if (nrow(results) == 0) {
    message("Couldn't tag any records.")
    return(NULL)
  }
  
  results
}


