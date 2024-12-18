# load libraries
#install.packages("quanteda")
library(quanteda)
library(dplyr)
library(openalexR)
library(stringr)

# TEST ON 815 (733) DATASET =============================================

# import data (send by Kaitlyn Hair)
load("data/oa_result.Rdata")

# rename dataframe
dat <- result
rm(result)

# merge title and abstract text
dat$text <- paste(dat$display_name, dat$ab, sep = ". ")

# remove any double periods
dat$text <- gsub("\\.\\.|\\. \\.", ".", dat$text)

# Check unique
dat <- dat %>% distinct()

# create corpus and tokenise
dat_tokens <- quanteda::tokens(quanteda::corpus(dat, 
                                                docid_field = "id", 
                                                text_field = "text"), 
                               what = "sentence")



# import regex dictionary
dictionary <- read.csv("countries.csv", stringsAsFactors = F)

# create empty dataframe for results
dat_results <- data.frame()

# run regexes on tokenised text
for (i in 1:nrow(dictionary)){
  try(dat_match <- quanteda::kwic(dat_tokens, dictionary$regex[i], 
                                  window = 1, valuetype = "regex"))
  if(!is.null(dat_match)){
    dat_match <- as.data.frame(dat_match)
    dat_match$pattern <- as.character(dat_match$pattern)
    dat_match <- dat_match %>%
      mutate(country = dictionary$country[i],
             continent = dictionary$continent[i],
             country_code = dictionary$institution_country_code[i],
             match = stringr::str_extract(dat_match$keyword, 
                                          dat_match$pattern))
    dat_results <- rbind(dat_results, dat_match)
  }
}

#format result output
dat_results <- dat_results %>%
  select(id = docname, sentence_number = from, sentence_text = keyword, pattern, 
         country, continent, country_code, match)

# TEST ON 10 MANUAL EXTRACTION ==========================================

# Create vdataframe of DOIs and manual annotations
dat_doi <- data.frame(doi = c("10.1111/add.14819", "10.1186/s13195-020-00587-5",
          "10.1093/brain/awaa034", "10.1002/ana.25669",
          "10.1098/rstb.2019.0144", "10.1096/fj.201901764R",
          "10.1007/s11427-019-1596-4", "10.1096/fj.201901758RR",
          "10.1007/s11427-019-9527-3", "10.1016/j.bbi.2019.05.016"),
          annotation = c("US", NA, NA, "US", NA,
                         "CN", "CN", "DE", NA, "FR"))

# Make DOI lower case
dat_doi$doi <- tolower(dat_doi$doi)

# Get openalex data and join with annotations
dat <- openalexR::oa_fetch(entity = "works", doi = dat$doi) %>%
  mutate(doi = stringr::str_remove(doi, "^https:\\/\\/doi\\.org\\/")) %>%
  select(id, doi, display_name, ab) %>%
  left_join(dat_doi, by = "doi")

# merge title and abstract text
dat$text <- paste(dat$display_name, dat$ab, sep = ". ")

# remove any double periods
dat$text <- gsub("\\.\\.|\\. \\.", ".", dat$text)

# remove any NAs for abstract
dat$text <- gsub("\\. NA", ".", dat$text)

# Check unique
dat <- dat %>% distinct()

# create corpus and tokenise
dat_tokens <- quanteda::tokens(quanteda::corpus(dat, 
                                                docid_field = "id", 
                                                text_field = "text"), 
                               what = "sentence")



# import regex dictionary
dictionary <- read.csv("countries.csv", stringsAsFactors = F)

# create empty dataframe for results
dat_results <- data.frame()

# run regexes on tokenised text
for (i in 1:nrow(dictionary)){
  try(dat_match <- quanteda::kwic(dat_tokens, dictionary$regex[i], 
                                  window = 1, valuetype = "regex"))
  if(!is.null(dat_match)){
    dat_match <- as.data.frame(dat_match)
    dat_match$pattern <- as.character(dat_match$pattern)
    dat_match <- dat_match %>%
      mutate(country = dictionary$country[i],
             continent = dictionary$continent[i],
             country_code = dictionary$institution_country_code[i],
             match = stringr::str_extract(dat_match$keyword, 
                                          dat_match$pattern))
    dat_results <- rbind(dat_results, dat_match)
  }
}

#format result output
dat_results <- dat_results %>%
  select(id = docname, sentence_number = from, sentence_text = keyword, pattern, 
         country, continent, country_code, match)

# link back with annotations
dat_results_annotation <- dat %>%
  select(id, doi, text, annotation) %>%
  left_join(dat_results, by = "id")
