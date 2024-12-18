# load libraries
#install.packages("quanteda")
library(quanteda)
library(dplyr)

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
