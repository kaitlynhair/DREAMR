format_doi <- function (df) {
  df$doi <- tolower(df$doi)
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("%28",
                                                                "(", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("%29",
                                                                ")", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("http://dx.doi.org/",
                                                                "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("https://doi.org/",
                                                                "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("https://dx.doi.org/",
                                                                "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("http://doi.org/",
                                                                "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi: ",
                                                                "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi:",
                                                                "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi",
                                                                "", x)))
  return(df)
}


#' Load in citations
#'
#' This function loads in a citation file within the shiny app
#' @import RefManageR
#' @importFrom glue glue
#' @import bibliometrix
#' @import XML
#' @importFrom utils read.csv read.table
#' @param paths Relative paths to the citations file or files
#' @param method  Import method
#' @param names File names of input file or files
#' @return A dataframe of the loaded citations.
#' @export

load_studies <-function(paths, names, method){

  df_list <- list()

  for (i in 1:length(paths)) {

    path <- paths[i]
    name <- names[i]

    # if(method == "bib"){
    # 
    #   # try wos format
    #   suppressMessages(suppressWarnings(try(newdat <- bibliometrix::convert2df(path, dbsource = "wos", format="bibtex"), silent=TRUE)))
    # 
    #   if(exists("newdat")){
    # 
    #     # Create a lookup table to map Field to Abbreviation
    #     lookup_table <- stats::setNames(field_codes_wos$Field, field_codes_wos$Abbreviation)
    # 
    #     # Rename the columns in df_original using the lookup_table
    #     colnames(newdat) <- lookup_table[colnames(newdat)]
    # 
    #     # Remove columns
    #     keep.cols <- names(newdat) %in% NA
    #     newdat <- newdat [! keep.cols]
    #     rownames(newdat) <- 1:nrow(newdat)
    # 
    #   }
    #   if(!exists("newdat")){
    # 
    #     # try pubmed format
    #     suppressMessages(suppressWarnings(try(newdat <- bibliometrix::convert2df(path, dbsource = "pubmed", format = "pubmed"), silent=TRUE)))
    # 
    #     if(exists("newdat")){
    # 
    #       # Create a lookup table to map Field to Abbreviation
    #       lookup_table <- setNames(field_codes_pubmed$Field, field_codes_pubmed$Abbreviation)
    # 
    #       # Rename the columns in df_original using the lookup_table
    #       colnames(newdat) <- lookup_table[colnames(newdat)]
    # 
    #       # Remove columns
    #       keep.cols <- names(newdat) %in% NA
    #       newdat <- newdat [! keep.cols]
    #       rownames(newdat) <- 1:nrow(newdat)
    # 
    #       # additional formatting for issn - keeping only ISSN vs other identifiers
    #       newdat$isbn <- trimws(stringr::str_extract(newdat$issn, ".{4}-.{4}.(?=\\((ELECTRONIC|PRINT\\)))"))
    #     }
    #   }
    # 
    #   if(!exists("newdat")){
    # 
    #     try(newdat <- RefManageR::ReadBib(path, check =FALSE))
    # 
    #   }
    # 
    #   newdat <- as.data.frame(newdat)
    # 
    #   cols <- c("author", "year", "journal", "doi", "pmid", "pmcid", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    #   newdat[cols[!(cols %in% colnames(newdat))]] = NA
    # 
    #   newdat$pages <- lapply(newdat$pages, function(x) gsub("--", "-", x))
    # 
    #   newdat$file_name <- name
    #   df_list[[i]] <- newdat
    # 
    # 
    #   remove(newdat)
    # 
    # }

    # if(method == "zotero_csv"){
    # 
    #   newdat <- utils::read.csv(path)
    #   newdat <- newdat %>%
    #     dplyr::rename(record_id = Key,
    #                   year = Publication.Year,
    #                   journal = Publication.Title,
    #                   keywords = Manual.Tags )
    # 
    #   names(newdat) <- tolower(names(newdat))
    # 
    #   cols <- c("author", "year", "journal", "doi", "pmid", "pmcid", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    #   newdat[cols[!(cols %in% colnames(newdat))]] = NA
    # 
    # 
    #   newdat$file_name <- name
    #   df_list[[i]] <- newdat
    # }

    # if(method == "ris"){
    # 
    #   newdat <- synthesisr::read_refs(path)
    # 
    #   cols <- c("author", "year", "journal", "doi", "pmid", "pmcid", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    #   newdat[cols[!(cols %in% colnames(newdat))]] = NA
    # 
    #   # rename or coalesce columns
    #   targets <- c("journal", "number", "pages", "isbn", "record_id", "booktitle")
    #   sources <- c("source", "issue", "start_page", "issn", "ID", "title")
    # 
    #   for (j in seq_along(targets)) {
    #     if (targets[j] %in% names(newdat)) {
    #       newdat[[targets[j]]] <- dplyr::coalesce(newdat[[targets[j]]], newdat[[sources[j]]])
    #     }  else {
    #       newdat[[targets[j]]] <- newdat[[sources[j]]]
    #     }}
    # 
    # 
    #   if ("end_page" %in% colnames(newdat)) {
    #     newdat <- newdat %>%
    #       dplyr::mutate(pages = .data$pages, "-", .data$end_page) %>%
    #       dplyr::select(-end_page)
    #   }
    # 
    #   newdat$pages <- lapply(newdat$pages, function(x) gsub("--", "-", x))
    # 
    # 
    #   newdat$file_name <- name
    #   df_list[[i]] <- newdat
    # }

    if(method == "endnote"){

      newdat<- XML::xmlParse(path)
      x <-  XML::getNodeSet(newdat,'//record')

      xpath2 <-function(x, ...){
        y <- XML::xpathSApply(x, ...)
        y <- gsub(",", "", y)  # remove commas if using comma separator
        ifelse(length(y) == 0, NA,  paste(y, collapse=", "))
      }

      newdat <- data.frame(
        author = sapply(x, xpath2, ".//author", xmlValue),
        year   = sapply(x, xpath2, ".//dates/year", xmlValue),
        journal = sapply(x, xpath2, ".//periodical/full-title", xmlValue),
        doi = sapply(x, xpath2, ".//electronic-resource-num", xmlValue),
        title = sapply(x, xpath2, ".//titles/title", xmlValue),
        pages = sapply(x, xpath2, ".//pages", xmlValue),
        volume = sapply(x, xpath2, ".//volume", xmlValue),
        number = sapply(x, xpath2, ".//number", xmlValue),
        abstract = sapply(x, xpath2, ".//abstract", xmlValue),
        record_id = sapply(x, xpath2, ".//rec-number", xmlValue),
        isbn = sapply(x, xpath2, ".//isbn", xmlValue),
        secondary_title = sapply(x, xpath2, ".//titles/secondary-title", xmlValue),
        accession_number = sapply(x, xpath2, ".//accession-num", xmlValue),
        keywords = sapply(x, xpath2, ".//keywords", xmlValue),
        type = sapply(x, xpath2, ".//ref-type", xmlValue),
        label = sapply(x, xpath2, ".//label", xmlValue),
        source = sapply(x, xpath2, ".//remote-database-name", xmlValue),
        url = sapply(x, xpath2, ".//urls/related-urls/url", xmlValue),
        database = sapply(x, xpath2, ".//remote-database-name", xmlValue)) %>%
        mutate(journal = ifelse(is.na(.data$journal), .data$secondary_title, .data$journal))

      cols <- c("author", "year", "journal", "doi", "pmid", "pmcid", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
      newdat[cols[!(cols %in% colnames(newdat))]] = NA

      newdat$file_name <- name
      df_list[[i]] <- newdat
    }

    if(method == "csv"){
      
      newdat <- utils::read.csv(path)
      
      # zotero-specific cols
      if(all(c("Key","Publication.Year","Publication.Title","Manual.Tags") %in% colnames(newdat))) {
        
        newdat <- newdat %>%
          dplyr::rename(record_id = Key,
                        year = Publication.Year,
                        journal = Publication.Title,
                        keywords = Manual.Tags )
      }
      
      # syrf-specific cols
      if(all(c("StudyId","PublicationName","CustomId") %in% colnames(newdat))) {
        
        newdat <- newdat %>%
          dplyr::rename(record_id = StudyId,
                        journal = PublicationName)
      }
      
      # pubmed-specific cols
      if(all(c("PMID","Publication.Year","Journal.Book") %in% colnames(newdat))) {
        
        newdat <- newdat %>%
          dplyr::rename(year = Publication.Year,
                        journal = Journal.Book)
      }
      
      
      names(newdat) <- tolower(names(newdat))
      
      if("authors" %in% colnames(newdat)) {
        newdat <- newdat %>% rename(author = authors) 
      }
      
      
      cols <- c("author", "year", "journal", "doi", "pmid", "pmcid", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
      names(newdat) <- tolower(names(newdat))
      
      if("authors" %in% colnames(newdat)) {
        newdat <- newdat %>% rename(author = authors) 
      }
      
      if("key" %in% colnames(newdat)) {
        newdat <- newdat %>% rename(record_id = key) 
      }
      
      cols <- c("author", "year", "journal", "doi", "pmid", "pmcid", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
      
      newdat[cols[!(cols %in% colnames(newdat))]] = NA
      
      newdat$file_name <- name
      
      df_list[[i]] <- newdat
      newdat[cols[!(cols %in% colnames(newdat))]] = NA
      
      newdat$file_name <- name
      
      df_list[[i]] <- newdat
      
      
    }
    
    if(method == "xlsx"){

      newdat <- openxlsx::read.xlsx(path, fillMergedCells = TRUE, skipEmptyRows = T, skipEmptyCols = T)
      
      names(newdat) <- tolower(names(newdat))
      
      if("authors" %in% colnames(newdat)) {
        newdat <- newdat %>% rename(author = authors) 
      }
      
      
      cols <- c("author", "year", "journal", "doi", "pmid", "pmcid", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
      
      newdat[cols[!(cols %in% colnames(newdat))]] = NA
      
      newdat$file_name <- name
      
      df_list[[i]] <- newdat
    }
    
  }

  # make sure year is character in all
  
    for (i in 1:length(df_list)) {
      
      # only run if col year exists
      if("year" %in% colnames(df_list[[i]])) {
        df_list[[i]]$year <- as.character(df_list[[i]]$year)
      }
      
    }
  

  newdat <- dplyr::bind_rows(df_list)

  cols_to_modify <-  c('author','title', 'year', 'journal', 'abstract', 'doi', "pmid", "pmcid",'number', 'pages', 'volume', 'isbn', 'record_id', 'label', 'source', 'url')
  
  # limit formatting to target columns that exist in input csv
  # actual_cols <- colnames(select(newdat, any_of(cols_to_modify)))
  
  newdat[cols_to_modify] <- lapply(newdat[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))

  return(newdat)

}
