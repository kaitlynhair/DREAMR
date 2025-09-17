library(dplyr)
library(tibble)

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

summarise_categorical <- function(x) {
  tab <- table(x, useNA = "ifany")
  pct <- round(100 * tab / sum(tab), 1)
  paste0(names(tab), ": ", tab, " (", pct, "%)")
}

summarise_author_role <- function(authors_df, role, var) {
  authors_df %>%
    filter(author_position == role) %>%
    pull({{var}}) %>%        # select the variable
    summarise_var()          # use your existing summarise_var
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
#' summary_df <- generate_summary_table(oa_results)
#' @export
generate_summary_table <- function(oa_results) {

  summary_table <- list(
    "Open access" = summarise_logical(oa_results$pub_metadata$is_oa),
    "Publication language" = summarise_categorical(oa_results$pub_metadata$language),
    "Publication year" = summarise_var(oa_results$pub_metadata$publication_year),
    "Number of journals" = as.character(length(unique(oa_results$pub_metadata$source_display_name))),
    "Field of research" = summarise_categorical(oa_results$pub_metadata$source_display_name),
    "Article type" = summarise_categorical(oa_results$pub_metadata$type),
    "Funding source specified" =  summarise_categorical(oa_results$pub_metadata$funder_name),
    "Number of authors per paper" = summarise_n_per_paper(oa_results$authors, paper_id_col = "openalex_id", inst_col = "author_id"),
    "Proportion female authors (first)" = "Unknown",
    "Proportion female authors (last)" = "Unknown",
    "Proportion female authors (all)" = "Unknown",
    "Years since first pub (all)" =  summarise_years_since_first_pub(oa_results$authors, role = c("first", "last","middle"), var = years_sice_first_pub),
    "Years since first pub (first)" = summarise_years_since_first_pub(oa_results$authors, role = "first", var = years_sice_first_pub),
    "Years since first pub (last)" = summarise_years_since_first_pub(oa_results$authors, role = "last", var = years_sice_first_pub),
    "Number of institutions per paper" = summarise_n_per_paper(oa_results$institutions, paper_id_col = "openalex_id", inst_col = "affilitation_id"),
    "Number of countries per paper" = summarise_n_per_paper(oa_results$institutions, paper_id_col = "openalex_id", inst_col = "country_code"),
    "Number of countries" = summarise_categorical(oa_results$institutions$country),
    "Type of institutions (all)" = summarise_author_role(oa_results$institutions,  role = c("first", "last", "middle"), var = type),
    "Type of institutions (first)" = summarise_author_role(oa_results$institutions, role = "first", var = type),
    "Type of institutions (last)" = summarise_author_role(oa_results$institutions, role = "last", var = type)
  )

  # Convert list into tidy data frame
  summary_df <- tibble::enframe(summary_table, name = "Characteristic", value = "Summary") %>%
    tidyr::unnest_longer(Summary)

  return(summary_df)
}

