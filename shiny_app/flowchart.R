# flowcharts.R
library(DiagrammeR)
library(dplyr)

# Helper: generate labels with counts + percentages
count_with_pct <- function(n_true, n_total, label) {
  pct <- ifelse(n_total > 0, round(100 * n_true / n_total, 1), 0)
  paste0(n_true, "/", n_total, " ", label, " (", pct, "%)")
}

# Flowchart generator
visualize_flow_3 <- function(labels){
  DiagrammeR::grViz(sprintf("
    digraph trials {
      graph [layout = dot, rankdir = TB, splines = false]
      node [shape = rectangle, width = 4.0, height = 0.8, fixedsize = true,
            penwidth = 1, fontname = Arial, fontsize = 10]  # wider nodes
      edge [penwidth = 1]

      one [label = '%s']
      two [label = '%s']
      three [label = '%s'] [color = black, fillcolor = lightgray,
                             fontcolor = black, shape = rounded, style='filled,dotted']

      one -> two -> three
    }
  ", labels[[1]], labels[[2]], labels[[3]]))
}


# References flowchart
render_flow_references <- function(rv) {
  req(rv$refdata, rv$oa_data)
  
  total_entries <- nrow(rv$refdata)
  with_id <- sum(!is.na(rv$refdata$doi) | !is.na(rv$refdata$pmid))
  retrieved <- sum(!is.na(rv$oa_data$pub_metadata$id))
  
  labels <- c(
    paste0(total_entries, " entries detected"),
    count_with_pct(with_id, total_entries, "references (DOIs and/or PMIDs)"),
    count_with_pct(retrieved, with_id, "OpenAlex records retrieved")
  )
  
  visualize_flow_3(labels)
}

# Institutions flowchart
render_flow_institutions <- function(rv) {
  req(rv$oa_data)

  inst_df <- rv$institutions
  n_total <- length(unique(inst_df$affilitation_id))
  n_with_ror <- length(unique(inst_df$affilitation_id[!is.na(inst_df$ror)]))
  n_country <- length(unique(inst_df$affilitation_id[!is.na(inst_df$country)]))
  
  labels <- c(
    paste0(n_total, " unique institutions in OpenAlex"),
    count_with_pct(n_with_ror, n_total, "institutions with ROR linkage"),
    count_with_pct(n_with_ror, n_total, "institutions mapped to country")
  )
  
  visualize_flow_3(labels)
}

# Authors flowchart
render_flow_authors <- function(rv) {
  req(rv$oa_data)
  
  auth_df <- rv$authors %>%
    distinct(author_id, .keep_all = TRUE)
  
  n_total <- nrow(auth_df)
  n_with_orcid <- sum(!is.na(auth_df$orcid))
  n_with_orcid_known_year <- sum(!is.na(auth_df$orcid) & auth_df$first_active_year != "Unknown")
  
  labels <- c(
    paste0(n_total, " unique first/last authors in OpenAlex"),
    count_with_pct(n_with_orcid, n_total, "first/last authors with ORCID"),
    count_with_pct(n_with_orcid_known_year, n_with_orcid, "first/last authors with minimum ORCID record")
  )
  
  visualize_flow_3(labels)
}