library(dplyr)
library(tibble)
library(ISOcodes)

# helper summariser
summarise_var <- function(x) {
  if (is.numeric(x)) {
    out <- paste0(
      "Median = ", round(median(x, na.rm=TRUE), 2),
      " (IQR: ",
      round(quantile(x, 0.25, na.rm=TRUE), 2), "–",
      round(quantile(x, 0.75, na.rm=TRUE), 2), ")"
    )
  } else {
    tab <- table(x, useNA="ifany")
    out <- paste0(names(tab), ": ", tab, " (",
                  round(100*tab/sum(tab),1), "%)")
  }
  as.character(out)  # <-- ensure all are character
}

summarise_logical <- function(x) {
  n_true <- sum(x, na.rm = TRUE)
  n_total <- length(x)
  pct_true <- round(100 * n_true / n_total, 1)
  paste0(n_true, "/", n_total, " (", pct_true, "%)")
}

summarise_categorical <- function(x, convert_fn = NULL) {
  
  x <- ifelse(is.na(x), "Information missing", x)
  
  # Apply conversion if provided (but not to "Information missing")
  if (!is.null(convert_fn)) {
    x <- ifelse(
      x == "Information missing",
      x,
      convert_fn(x)
    )
  }
  
  tab <- table(x)
  n_total <- length(x)
  
  nm <- names(tab)
  is_missing <- nm == "Information missing"
  
  # Missing first, then ascending count, then alphabetical tie-break
  ord <- c(
    which(is_missing),
    which(!is_missing)[order(tab[!is_missing], nm[!is_missing])]
  )
  
  tab <- tab[ord]
  
  pct <- round(100 * tab / sum(tab), 1)
  vals <- paste0(names(tab), ": ", tab, "/", n_total, " (", pct, "%)")
  
  paste(vals, collapse = "<br>")
}

convert_lang <- function(x) {
  lookup <- ISOcodes::ISO_639_2
  
  out <- lookup$Name[match(x, lookup$Alpha_2)]
  
  ifelse(is.na(out), x, out)
}

summarise_country <- function(x, metadata){
  
  len <- length(unique(metadata$id))
  
  df <- x %>%
    dplyr::select(openalex_id, country) %>%
    dplyr::mutate(
      country = ifelse(is.na(country), "Information missing", country)
    ) %>%
    dplyr::distinct() %>%               
    dplyr::count(country, name = "n") %>%           
    dplyr::mutate(pct = round(100 * n / len, 1))
  
  # Order results
  df <- df %>%
    dplyr::mutate(
      missing_first = country != "Information missing"
    ) %>%
    dplyr::arrange(missing_first, n, country) %>%
    dplyr::select(-missing_first)
  
  summary <- paste0(
    df$country, ": ", 
    df$n, "/", len, 
    " (", df$pct, "%)"
  )
  
  paste(summary, collapse = "<br>")
}
  
summarise_author_role <- function(authors_df, role, var, include_only = NULL) {
  
  # Filter by role
  vals <- authors_df %>%
    filter(author_position == role) %>%
    pull({{ var }})
  
  # Handle categorical variables with optional filtering
  if (!is.numeric(vals)) {
    
    # Replace NA with "Information missing"
    vals <- ifelse(is.na(vals), "Information missing", vals)
    
    tab <- table(vals, useNA = "ifany")
    n_total <- length(vals)
    pct <- round(100 * tab / sum(tab), 1)
    
    df <- data.frame(
      category = names(tab),
      count = as.numeric(tab),
      pct = as.numeric(pct),
      stringsAsFactors = FALSE
    )
    
    # Optionally filter categories
    if (!is.null(include_only)) {
      df <- df %>% dplyr::filter(category %in% include_only)
    }
    
    # Order: Information missing first, then ascending count, then alphabetical
    df <- df %>%
      dplyr::mutate(
        missing_first = category != "Information missing"
      ) %>%
      dplyr::arrange(missing_first, count, category) %>%
      dplyr::select(-missing_first)
    
    summary <- paste0(df$category, ": ", df$count, "/", n_total, " (", df$pct, "%)")
    return(paste(summary, collapse = "<br>"))
  }
  
  # Otherwise just use numeric summariser
  return(summarise_var(vals))
}
summarise_n_per_paper <- function(authors_df, paper_id_col, inst_col) {
  
  authors_df %>%
    # select only paper ID and institution columns, remove duplicates
    select(!!sym(paper_id_col), !!sym(inst_col)) %>%
    distinct() %>%
    # count unique institutions per paper
    group_by(!!sym(paper_id_col)) %>%
    summarise(n_institutions = n(), .groups = "drop") %>%
    pull(n_institutions) %>%           # extract numeric vector
    {                                  # compute median (IQR)
      median_val <- median(., na.rm = TRUE)
      q25 <- quantile(., 0.25, na.rm = TRUE)
      q75 <- quantile(., 0.75, na.rm = TRUE)
      paste0(median_val, " (", q25, "–", q75, ")")
    }
}

summarise_years_since_first_pub <- function(authors_df, role, var) {
  authors_df %>%
    filter(author_position %in% role) %>%         # filter by role
    pull({{var}}) %>%                           # select the variable
    {                                           # handle unknowns and compute median/IQR
      x_num <- as.numeric(ifelse(. == "Unknown", NA, .))
      q25 <- quantile(x_num, 0.25, na.rm = TRUE)
      q75 <- quantile(x_num, 0.75, na.rm = TRUE)
      median_val <- median(x_num, na.rm = TRUE)
      paste0(median_val, " (", q25, "–", q75, ")")
    }
}

summarise_years_since_first_pub_all <- function(authors_df, var) {
  first <- summarise_years_since_first_pub(authors_df, role = "first", var = {{var}})
  last  <- summarise_years_since_first_pub(authors_df, role = "last", var = {{var}})
  all   <- summarise_years_since_first_pub(authors_df, role = c("first","last","middle"), var = {{var}})
  
  paste0("First: ", first,
         "<br>Last: ", last,
         "<br>All: ", all)
}

summarise_years_since_first_pub_all <- function(authors_df, var) {
  first <- summarise_years_since_first_pub(authors_df, role = "first", var = {{var}})
  last  <- summarise_years_since_first_pub(authors_df, role = "last", var = {{var}})
  all   <- summarise_years_since_first_pub(authors_df, role = c("first","last","middle"), var = {{var}})
  
  paste0("First: ", first,
         "<br>Last: ", last,
         "<br>All: ", all)
}


compute_completeness <- function(x) {
  mean(!is.na(x) & x != "")
}

# Example: manual overrides
manual_trust <- list(
  "Publication language" = 60,   # you know this one is dodgy
  "Funding source specified" = 50,
  "Article type" = 60,
  "Proportion female authors (first author)" = 70,
  "Proportion female authors (last author)" = 70
)


# Function to compute completeness-based trust (0–100 scale)
# Compute trust based on completeness (for columns we know)
# Trust icons with colors
compute_trust_icon <- function(oa_results, characteristic, manual_trust) {
 
  # manual override first
  if (characteristic %in% names(manual_trust)) {
    trust_val <- manual_trust[[characteristic]]
  } else {
    # map characteristics to source
    col_map <- list(
      "Open access" = list(df = "pub_metadata", col = "is_oa"),
      "Publication language" = list(df = "pub_metadata", col = "language"),
      "Publication year" = list(df = "pub_metadata", col = "publication_year"),
      "Funding source specified" = list(df = "pub_metadata", col = "funder_name"),
      "Number of journals" = list(df = "pub_metadata", col = "source_display_name"),
      "Field of research" = list(df = "pub_metadata", col = "domain"),
      "Article type" = list(df = "pub_metadata", col = "type"),
      "Number of authors per paper" = list(df = "authors", col = "author_id"),
      "Proportion female authors (first author)" = list(df = "authors", col = "author_id"),
      "Proportion female authors (last author)" = list(df = "authors", col = "author_id"),
      "Years since first publication" = list(df = "authors", col = "years_sice_first_pub"),
      "Number of institutions per paper" = list(df = "institutions", col = "affilitation_id"),
      "Number of countries per paper" = list(df = "institutions", col = "country_code"),
      "Number of countries" = list(df = "institutions", col = "country"),
      "Type of institutions (first author)" = list(df = "institutions", col = "type"),
      "Type of institutions (last author)" = list(df = "institutions", col = "type")
    )
    
    trust_val <- NA_real_
    if (characteristic %in% names(col_map)) {
      map_info <- col_map[[characteristic]]
      df <- oa_results[[map_info$df]]
      col <- map_info$col
      if (!is.null(df) && col %in% names(df)) {
        completeness <- sum(!is.na(df[[col]])) / nrow(df)
        trust_val <- round(100 * completeness, 1)
      }
    }
  }
  
  # Return icon HTML
  if (is.na(trust_val)) {
    return('<i class="fa fa-solid fa-triangle-exclamation" style="color: orange;"></i>') # unknown
  } else if (trust_val >= 80) {
    return('<i class="fa fa-check-circle" style="color: green;"></i>')   # good
  } else if (trust_val >= 45) {
    return('<i class="fa fa-exclamation-triangle" style="color: red;"></i>') # warning
  } else {
    return('<i class="fa fa-exclamation-triangle" style="color: red;"></i>')     # red warning X
  }
}


#' Generate Paper Summary Table from OA Results
#'
#' Summarizes key characteristics of papers and authors from an OpenAlex-like
#' dataset, including open access status, publication info, author stats,
#' institutions, and countries.
#'
#' @param oa_results A list containing paper metadata, author metadata, and institution metadata.
#'   Expected elements:
#'   - `pub_metadata`: Data frame with columns like `is_oa`, `language`, `publication_year`, `source_display_name`, `type`, `funder_name`.
#'   - `authors`: Data frame with author-level data including `years_sice_first_pub`, `role`, `paper_id`.
#'   - `institutions`: Data frame with institution-level data including `paper_id`, `affiliation_id`, `country_code`, `type`, and `role`.
#' @return A tibble with two columns: `Characteristic` and `Summary`, suitable for tables or Shiny display.
#' @examples
#' summary_df <- generate_summary_table(oa_results, total_citations = 120)
#' @export
generate_summary_table <- function(oa_results, total_citations = NULL) {   # total_citations serves no purpose?
  
  # Optional overall retrieval summary
  found_n <- if (!is.null(oa_results$pub_metadata)) nrow(oa_results$pub_metadata) else 0

   browser()
  summary_table <- c(list(
    "Open access" = summarise_logical(oa_results$pub_metadata$is_oa),
    "Publication language" = summarise_categorical(oa_results$pub_metadata$language, convert_fn=convert_lang),
    "Publication year" = summarise_var(oa_results$pub_metadata$publication_year),
    "Number of journals" = as.character(length(unique(oa_results$pub_metadata$source_display_name))),
    "Field of research" = summarise_categorical(oa_results$pub_metadata$domain),
    "Article type" = summarise_categorical(oa_results$pub_metadata$type),
    "Funding source specified" =  summarise_categorical(oa_results$pub_metadata$funder_name),
    "Number of authors per paper" = summarise_n_per_paper(oa_results$authors, paper_id_col = "openalex_id", inst_col = "author_id"),
    "Proportion female authors (first author)" =summarise_author_role(oa_results$authors, role ="first", var="gender", include_only="female"),
    "Proportion female authors (last author)" = summarise_author_role(oa_results$authors, role ="last", var="gender", include_only="female"),
    "Years since first publication" = summarise_years_since_first_pub_all(oa_results$authors, var = years_sice_first_pub),
    "Number of institutions per paper" = summarise_n_per_paper(oa_results$institutions, paper_id_col = "openalex_id", inst_col = "affilitation_id"),
    "Number of countries per paper" = summarise_n_per_paper(oa_results$institutions, paper_id_col = "openalex_id", inst_col = "country_code"),
    "Number of countries" = summarise_country(oa_results$institutions, oa_results$pub_metadata),
    "Type of institutions (first author)" = summarise_author_role(oa_results$institutions, role = "first", var = type),
    "Type of institutions (last author)" = summarise_author_role(oa_results$institutions, role = "last", var = type)
  ))

  # Add trust scores
  # Default: based on completeness
  
  summary_df <- tibble::enframe(summary_table, name = "Characteristic", value = "Summary") %>%
    tidyr::unnest_longer(Summary) %>%
    rowwise() %>%
    mutate(Trust = compute_trust_icon(oa_results, Characteristic, manual_trust)) %>%
    ungroup()
}

