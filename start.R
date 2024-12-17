library(dplyr)
source("R/oa_metadata.R")

dat <- read_excel("data/shortcut_citations/dataset_all_fields_prevalence.xlsx")
test <- dat[1:20,]

test <- test %>%
  select(title, author, pubmed_id, url) %>%
  mutate(pmid = paste0("pmid:", pubmed_id))

result <- oa_metadata(test, identifier="pmid")

# get institutions
institutions <- extract_institution(result)

# get funders
funders <- extract_funder(result)
