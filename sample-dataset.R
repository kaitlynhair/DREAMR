library(dplyr)

# open alex functions
source("R/oa_metadata.R")

# read in example data
dat <-readxl::read_excel("data/shortcut_citations/dataset_all_fields_prevalence.xlsx")

test <- dat %>%
  select(title, author, pubmed_id, url) %>%
  mutate(pmid = paste0("pmid:", pubmed_id))

result <- oa_metadata(test, identifier="pmid")

# how much publication language?
language <- result %>% group_by(id) %>% select(language, id) %>% unique()

# get institutions
institutions_first <- extract_institution(result, author_position="first")
institutions_first <- institutions_first %>%
  filter(!institution_id =="Unknown") %>%
  group_by(id) %>%
  slice_head()
length(unique(institutions_first$doi))

# get institutions last
institutions_last <- extract_institution(result,  author_position="last")
institutions_last <- institutions_last %>%
  filter(!institution_id =="Unknown") %>%
  group_by(id) %>%
  slice_head()
length(unique(institutions_last$doi))

# get funders
funders <- extract_funder(result)
funders <- soles::format_doi(funders)
funders <- funders %>%
  filter(!funder_name =="Unknown") %>%
  group_by(id) %>%
  slice_head()
length(unique(funders$doi))

# how many abstracts
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

# get institutions
institutions_first_author <- extract_institution(result_checked, author_position = "first")
institutions_first_author <- institutions_first_author %>%
  filter(!institution_id =="Unknown") %>%
  group_by(id) %>%
  slice_head() %>%
  select(name, doi, institution_country_code) %>%
  rename(last_author_institution = name, first_author_country=institution_country_code)

# get institutions
institutions_last_author <- extract_institution(result_checked, author_position = "last")
institutions_last_author <- institutions_last_author %>%
  filter(!institution_id =="Unknown") %>%
  group_by(id) %>%
  slice_head() %>%
  select(name, doi, institution_country_code) %>%
  rename(last_author_institution = name, last_author_country=institution_country_code)

first_author_career_stage <- result_checked %>%
  tidyr::unnest(author) %>%
  select(id, au_id, publication_year, author_position, au_orcid) %>%
  filter(author_position=="first")

last_author_career_stage <- result_checked %>%
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

# how many global south?
# NOTE: there is nothing called institutions
global_south <- institutions %>%
  mutate(is_global_south = ifelse(institution_country_code %in% global_south_country_codes, TRUE, FALSE)) %>%
  select(doi, institution_id, is_global_south) %>%
  unique()

institutions_first <- left_join(institutions_first, global_south) %>% rename(is_first_author_global_south = is_global_south) %>%select(is_first_author_global_south, doi)
institutions_last <- left_join(institutions_last, global_south) %>% rename(is_last_author_global_south = is_global_south) %>% select(is_last_author_global_south, doi)

result_checked_final <- result_checked %>%
  select(title, doi, id, is_oa, language, cited_by_count) %>%
  left_join(first_author_career_stage, by="id") %>%
  left_join(last_author_career_stage, by="id") %>%
  left_join(institutions_last_author) %>%
  left_join(institutions_first_author) %>%
  left_join(institutions_last) %>%
  left_join(institutions_first) %>%
  left_join(funders) %>%
  unique()

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
