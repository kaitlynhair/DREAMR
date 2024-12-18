library(dplyr)
source("R/oa_metadata.R")

dat <-readxl::read_excel("data/shortcut_citations/dataset_all_fields_prevalence.xlsx")

test <- dat %>%
  select(title, author, pubmed_id, url) %>%
  mutate(pmid = paste0("pmid:", pubmed_id))

result <- oa_metadata(test, identifier="pmid")

# how much publication language?
language <- result %>% group_by(doi) %>% select(language, id) %>% unique()

# get institutions
institutions <- extract_institution(result)
institutions_sliced <- institutions %>%
  filter(!institution_id =="Unknown") %>%
  group_by(doi) %>%
  slice_head()
length(unique(institutions_sliced$doi))

save(authors, file="authors.Rdata")

# get funders
funders <- extract_funder(result)
funders <- soles::format_doi(funders)
funders_sliced <- funders %>%
  filter(!funder_name =="Unknown") %>%
  group_by(doi) %>%
  slice_head()
length(unique(funders_sliced$doi))


# abstracts
abstracts <- result %>%
  select(id, ab) %>%
  filter(!ab=="") %>%
  unique()

# sample of 10
set.seed(101)
ten <- dat[1:10,]
random <- dat[sample(nrow(dat), 10), ]
random <- random %>% select(title, author, year, url, pubmed_id)

# After validation check
check <- test %>% filter(pubmed_id %in% c("31599027","32131891", "32203578",
                                     "31916270","31928188","31953872",
                                     "32048161","31957074","31152389",
                                     "31100369"))
result_checked <- oa_metadata(check, identifier="pmid")
result_checked <- result_checked %>%
  select()

# get institutions
institutions_first_author <- extract_institution(result_checked, author_position = "first")
institutions_first_author_sliced <- institutions_first_author %>%
  filter(!institution_id =="Unknown") %>%
  group_by(doi) %>%
  slice_head() %>%
  select(name, doi) %>%
  rename(first_author_institution = name)

# get institutions
institutions_last_author <- extract_institution(result_checked, author_position = "last")
institutions_last_author_sliced <- institutions_last_author %>%
  filter(!institution_id =="Unknown") %>%
  group_by(doi) %>%
  slice_head() %>%
  select(name, doi) %>%
  rename(last_author_institution = name)

funders <- extract_funder(result_checked)

first_author_career_stage <- result_checked %>%
  tidyr::unnest(author) %>%
  select(id, counts_by_year, au_display_name, au_id, author_position) %>%
  tidyr::unnest(counts_by_year) %>%
  filter(author_position=="first") %>%
  group_by(au_id) %>%
  arrange(year) %>%
  mutate(first_author_active = as.numeric(format(Sys.Date(), "%Y")) - first(year)) %>%
  select(-year, -cited_by_count, -author_position) %>%
  unique() %>%
  ungroup()

last_author_career_stage <- result_checked %>%
  tidyr::unnest(author) %>%
  select(id, counts_by_year, au_display_name, au_id, author_position) %>%
  tidyr::unnest(counts_by_year) %>%
  filter(author_position=="last") %>%
  group_by(au_id) %>%
  arrange(year) %>%
  mutate(last_author_active = as.numeric(format(Sys.Date(), "%Y")) - first(year)) %>%
  select(-year, -cited_by_count, -author_position, -authors, -au_id) %>%
  unique() %>%
  ungroup()

result_checked_final <- result_checked %>%
  select(title, doi, id) %>%
  left_join(first_author_career_stage) %>%
  left_join(last_author_career_stage) %>%
  left_join(institutions_last_author) %>%
  left_join(institutions_first_author)
